
use std::fmt::{Debug, Display};
use std::ops::AddAssign;
use num_traits::{Num};
use serde::{Serialize, Serializer};
use serde::ser::SerializeStruct;
use getset::{CopyGetters, WithSetters};

// const UNREACHABLE_MESSAGE: &str = "If you see this line of text, please inform the author to modify the code.";
pub const MAX_WATER_LEVEL: usize = 79;

const UP_NAMES: [[&str; 6]; 2] = [
    [
        "本次抽出第1个up",
        "本次抽出第2个up",
        "本次抽出第3个up",
        "本次抽出第4个up",
        "本次抽出第5个up",
        "本次抽出第6个up",
    ],
    [
        "累计抽出第1个up",
        "累计抽出第2个up",
        "累计抽出第3个up",
        "累计抽出第4个up",
        "累计抽出第5个up",
        "累计抽出第6个up",
    ],
];
const DEFAULT_UP_NAME: [&str; 2] = ["本次抽出第N个up", "累计抽出第N个up"];

#[derive(Debug, Default, CopyGetters, WithSetters)]
pub struct EndFiledGacheCalculatorOption {
    #[getset(get_copy = "pub", set_with = "pub")]
    bonus_at_30: bool,
    #[getset(get_copy = "pub", set_with = "pub")]
    bonus_at_each_240: bool,
    #[getset(get_copy = "pub", set_with = "pub")]
    pity_at_120: bool,
    #[getset(get_copy = "pub", set_with = "pub")]
    current_water_level: usize,
    #[getset(get_copy = "pub", set_with = "pub")]
    curren_count: usize,
    #[getset(get_copy = "pub", set_with = "pub")]
    current_up_count: usize,
}

impl EndFiledGacheCalculatorOption {
    pub const fn new() -> Self {
        Self {
            bonus_at_30: true,
            bonus_at_each_240: true,
            pity_at_120: true,
            current_water_level: 0,
            curren_count: 0,
            current_up_count: 0,
        }
    }
}

/// 用以分布计算，实现了迭代器 trait，其中 N 是所需的 up 数量
#[derive(Debug)]
pub struct EndFiledGacheCalculator<T, const N: usize>
where
    T: Num + Debug + Display + Clone + PartialOrd + AddAssign,
{
    probabilities: [T; 15],
    count: usize,
    distribution: [[T; MAX_WATER_LEVEL + 1]; N],
    zeros: [[T; MAX_WATER_LEVEL + 1]; N],
    completed: T,
    total_probabilites: [T; N],
    option: EndFiledGacheCalculatorOption,
}

impl<T, const N: usize> Default for EndFiledGacheCalculator<T, N>
where
    T: Num + Debug + Display + Clone + PartialOrd + AddAssign,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<T, const N: usize> EndFiledGacheCalculator<T, N>
where
    T: Num + Debug + Display + Clone + PartialOrd + AddAssign,
{
    pub fn new() -> Self {
        Self::with_params(EndFiledGacheCalculatorOption::new())
    }

    pub fn with_params(option: EndFiledGacheCalculatorOption) -> Self {
        assert!(N > 0, "N must be greater than 0.");
        assert!(option.current_water_level() < MAX_WATER_LEVEL, "Water level must be less than {}.", MAX_WATER_LEVEL);

        let current_water_level = option.current_water_level;
        let count = option.curren_count;
        let current_up_count = option.current_up_count;
        let zeros_row: [T; MAX_WATER_LEVEL + 1] = (0..MAX_WATER_LEVEL + 1).map(|_| T::zero()).collect::<Vec<_>>().try_into().unwrap();
        let zeros: [[T; MAX_WATER_LEVEL + 1]; N] = (0..N).map(|_| zeros_row.clone()).collect::<Vec<[T; MAX_WATER_LEVEL + 1]>>().try_into().unwrap();
        let probabilities = {
            let mut v = Vec::with_capacity(20);
            let mut x = Self::base();
            for _ in 0..14 {
                v.push(x.clone());
                x += Self::delta();
            }
            v.push(T::one());
            v.try_into().unwrap()
        };
        let mut distribution = zeros.clone();
        distribution[current_up_count][current_water_level] = T::one();
        let completed = T::zero();
        let total_probabilites = (0..N).map(|_| T::zero()).collect::<Vec<_>>().try_into().unwrap();

        Self { probabilities, count, distribution, zeros, completed, total_probabilites, option }
    }

    /// 六星基础概率
    fn base() -> T {
        let five = T::one() + T::one() + T::one() + T::one() + T::one();
        T::one() / five.clone() / five.clone() / five
    }

    /// 六星递增概率
    fn delta() -> T {
        let two = T::one() + T::one();
        let five = two.clone() + two.clone() + T::one();
        T::one() / two.clone() / two / five
    }

    /// 用水位取得对应的六星概率
    fn probability(&self, water_level: usize) -> T {
        self.probabilities[water_level.saturating_sub(65)].clone()
    }

    /// 用水位取得对应的 六星up 六星非up 非六星 概率
    fn probabilities(&self, water_level: usize) -> (T, T, T) {
        let rank_6 = self.probability(water_level);
        let not_rank_6 = T::one() - rank_6.clone();
        let rank_6_up = rank_6 / (T::one() + T::one());
        let rank_6_not_up = rank_6_up.clone();
        (rank_6_up, rank_6_not_up, not_rank_6)
    }

    /// 不考虑保底的单抽，用来处理一些特殊机制
    fn gacha_with(&mut self, rank_6_up: T, rank_6_not_up: T, not_rank_6: T, current_probabilites: &mut [T; N]) {
        #[cfg(debug_assertions)]
        assert!(Self::approximately_eq(rank_6_up.clone() + rank_6_not_up.clone() + not_rank_6.clone(), T::one()));

        let zeros = self.zeros.clone();
        let lasts = std::mem::replace(&mut self.distribution, zeros);

        for (up_got_count, lasts_row) in lasts.into_iter().enumerate() {
            for (water_level, last) in lasts_row.into_iter().enumerate() {
                // 未抽出六星，水位不变
                self.distribution[up_got_count][water_level] += last.clone() * not_rank_6.clone();
                // 六星歪，水位不变
                self.distribution[up_got_count][water_level] += last.clone() * rank_6_not_up.clone();
                // 六星up，水位不变
                *if up_got_count + 1 < N {
                    // 未抽齐
                    current_probabilites[up_got_count] += last.clone() * rank_6_up.clone();
                    &mut self.distribution[up_got_count + 1][water_level]
                } else {
                    // 抽齐了
                    &mut current_probabilites[N - 1]
                } += last * rank_6_up.clone();
            }
        }
    }

    #[cfg(debug_assertions)]
    fn epsilon() -> T {
        let mut t = T::one() + T::one();
        for _ in 0..5 {
            let t_2 = t.clone();
            t = t * t_2;
        }
        T::one() / t
    }

    #[cfg(debug_assertions)]
    fn approximately_eq(a: T, b: T) -> bool {
        a == b
        || {
            let diff = if a > b {
                a - b
            } else {
                b - a
            };
            diff < Self::epsilon()
        }
    }
}

#[derive(Debug)]
pub struct SingleGachaOutcome<T, const N: usize>
where
    T: Num + Debug + Display + Clone + PartialOrd + AddAssign,
{
    count: usize,
    current_probabilites: [T; N],
    total_probabilites: [T; N],
    remark: String,
}

impl<T, const N: usize> Serialize for SingleGachaOutcome<T, N>
where
    T: Num + Debug + Display + Clone + PartialOrd + AddAssign,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
         S: Serializer,
    {
        let mut state = serializer.serialize_struct("ProbabilityRecord", 2 + N * 2)?;

        // 序列化次数
        state.serialize_field("次数", &self.count)?;
        state.serialize_field("备注", self.remark.as_str())?;

        // 序列化概率字段，使用常量名称
        for (i, datas) in [&self.current_probabilites, &self.total_probabilites].into_iter().enumerate() {
            for (index, prob) in datas.iter().enumerate() {
                let field_name = if index < 6 {
                    UP_NAMES[i][index]
                } else {
                    DEFAULT_UP_NAME[i]
                };
                state.serialize_field(field_name, &prob.to_string())?;
            }
            if i == 0 {
                state.serialize_field("", "")?;
            }
        }

        state.end()
    }
}

impl<T, const N: usize> Iterator for EndFiledGacheCalculator<T, N>
where
    T: Num + Debug + Display + Clone + PartialOrd + AddAssign,
{
    type Item = SingleGachaOutcome<T, N>;

    fn next(&mut self) -> Option<Self::Item> {
        self.count += 1;

        let zeros = self.zeros.clone();
        let lasts = std::mem::replace(&mut self.distribution, zeros);
        let mut current_probabilites: [T; N] = (0..N).map(|_| T::zero()).collect::<Vec<T>>().try_into().unwrap();
        let mut remark = String::new();
        
        for (up_got_count, lasts_row) in lasts.into_iter().enumerate() {
            for (water_level, last) in lasts_row.into_iter().enumerate() {
                let (rank_6_up, rank_6_not_up, not_rank_6) = if !(self.option.pity_at_120() && self.count == 120 && up_got_count == 0) {
                    self.probabilities(water_level)
                } else {
                    // 120抽第一个保底up
                    if remark.is_empty() {
                        remark = String::from("触发 120 抽保底");
                    }
                    (T::one(), T::zero(), T::zero())
                };
                #[cfg(debug_assertions)]
                assert!(Self::approximately_eq(rank_6_up.clone() + rank_6_not_up.clone() + not_rank_6.clone(), T::one()));

                // 未抽出六星，水位 + 1
                self.distribution[up_got_count][(water_level + 1).min(MAX_WATER_LEVEL)] += last.clone() * not_rank_6;
                // 六星歪
                self.distribution[up_got_count][0] += last.clone() * rank_6_not_up;
                // 六星up
                *if up_got_count + 1 < N {
                    // 未抽齐
                    current_probabilites[up_got_count] += last.clone() * rank_6_up.clone();
                    &mut self.distribution[up_got_count + 1][0]
                } else {
                    // 抽齐了
                    &mut current_probabilites[N - 1]
                } += last * rank_6_up;
            }
        }

        if self.option.bonus_at_30() && self.count == 30 {
            // 第 30 抽送的十连直接计算在第 30 抽之内
            for _ in 0..10 {
                self.gacha_with(Self::base() / (T::one() + T::one()), Self::base() / (T::one() + T::one()), T::one() - Self::base(), &mut current_probabilites);
            }
            remark = String::from("触发 30 抽赠送十连");
        } else if self.option.bonus_at_each_240() && self.count.is_multiple_of(240) {
            // 累计 240 * n 送的潜能直接计算在第 240 * n 抽之内
            self.gacha_with(T::one(), T::zero(), T::zero(), &mut current_probabilites);
            remark = String::from("触发每 240 抽赠送潜能");
        }

        self.completed += current_probabilites[N - 1].clone();
        self.total_probabilites.iter_mut().zip(&current_probabilites).for_each(|(a, b)| *a += b.clone());
        let count = self.count;
        let total_probabilites = self.total_probabilites.clone();

        Some(SingleGachaOutcome { count, current_probabilites, total_probabilites, remark })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use num_rational::BigRational;

    #[test]
    fn get_base_and_delta() {
        let base = EndFiledGacheCalculator::<BigRational, 7>::base();
        dbg!(base);
        let delta = EndFiledGacheCalculator::<BigRational, 7>::delta();
        dbg!(delta);
    }

    #[test]
    fn test_epsilon() {
        let e = EndFiledGacheCalculator::<f64, 7>::epsilon();
        dbg!(e);
    }

    #[test]
    fn case_1() {
        let file = std::fs::File::options()
            .write(true)
            .create(true)
            .truncate(true)
            .open("output_rational.csv")
            .unwrap();
        let mut wtr = csv::Writer::from_writer(file);
        let mut iter: EndFiledGacheCalculator<BigRational, 6> = EndFiledGacheCalculator::new();
        for _ in 1..=10 {
            let outcome = iter.next().unwrap();
            dbg!(&outcome);
            wtr.serialize(&outcome).unwrap();
        }
    }

    #[test]
    fn case_2() {
        let file = std::fs::File::options()
            .write(true)
            .create(true)
            .truncate(true)
            .open("output_f64.csv")
            .unwrap();
        let mut wtr = csv::Writer::from_writer(file);
        let mut iter: EndFiledGacheCalculator<f64, 6> = EndFiledGacheCalculator::new();
        for _ in 1..=1200 {
            let outcome = iter.next().unwrap();
            dbg!(&outcome);
            wtr.serialize(&outcome).unwrap();
        }
    }

    #[test]
    fn case_3() {
        let file = std::fs::File::options()
            .write(true)
            .create(true)
            .truncate(true)
            .open("output_f64_one.csv")
            .unwrap();
        let mut wtr = csv::Writer::from_writer(file);
        let mut iter: EndFiledGacheCalculator<f64, 1> = EndFiledGacheCalculator::new();
        for _ in 1..=1200 {
            let outcome = iter.next().unwrap();
            dbg!(&outcome);
            wtr.serialize(&outcome).unwrap();
        }
    }

    #[test]
    fn test_option() {
        let _option = EndFiledGacheCalculatorOption::default()
            .with_bonus_at_30(false)
            .with_pity_at_120(false);
    }
}