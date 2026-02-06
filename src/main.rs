use endfield_gacha_calculator_lib::{EndFiledGacheCalculator, EndFiledGacheCalculatorOption};
use std::io::Write;
use num_rational::BigRational;
use clap::Parser;
use indicatif::{ProgressBar, ProgressStyle};

#[derive(Parser, Debug)]
#[command(
    version,
    about = "终末地抽卡计算器",
    arg_required_else_help = true,
)]
struct Args {
    /// 当前已抽次数
    #[arg(short, long, default_value_t = 0)]
    count: usize,

    /// 当前水位
    #[arg(short, long, default_value_t = 0)]
    water_level: usize,

    /// 已抽到的 up 数
    #[arg(short, long, default_value_t = 0)]
    up_count: usize,

    /// 总计要抽的 up 数
    #[arg(long, default_value_t = 6)]
    total_up_count: usize,

    /// 总计要抽的次数
    #[arg(short, long, default_value_t = 120)]
    total_count: usize,

    /// 不考虑第 30 抽赠送十连
    #[arg(long)]
    no_bonus_at_30: bool,

    /// 不考虑 120 抽保底
    #[arg(long)]
    no_pity_at_120: bool,

    /// 不考虑每 240 抽赠送潜能
    #[arg(long)]
    no_bonus_at_each_240: bool,

    /// 使用有理数结果
    #[arg(long)]
    rational: bool,
}

macro_rules! run_gacha_simulation {
    ($option:expr, $total_count:expr, $total_up_count:expr, $num_type:ty, $wtr:expr, $pb:expr) => {{
        let mut iter = EndFiledGacheCalculator::<$num_type, $total_up_count>::with_params($option);
        for _ in 1..=$total_count {
            let outcome = iter.next().expect("iterator exhausted early");
            $pb.inc(1);
            $wtr.serialize(&outcome).unwrap();
        }
        $pb.finish();
        println!("Completed.");
    }};
}

macro_rules! match_params {
    ($option:expr, $total_count:expr, $total_up_count:expr, $num_type:ty, $wtr:expr, $pb:expr, $($n:literal),+) => {
        match $total_up_count {
            $(
                $n => run_gacha_simulation!($option, $total_count, $n, $num_type, $wtr, $pb),
            )+
            _ => unimplemented!(),
        }
    };
}

fn main() {
    let Args { count, water_level, up_count, total_up_count, total_count, no_bonus_at_30, no_pity_at_120, no_bonus_at_each_240, rational } = Args::parse();

    let path = format!("output_{}_{}_{}_{}_{}{}{}{}{}.csv", count, water_level, up_count, total_up_count, total_count,
        if no_bonus_at_30 { "_without-30" } else { Default::default() },
        if no_pity_at_120 { "_without-120" } else { Default::default() },
        if no_bonus_at_each_240 { "_without-240" } else { Default::default() },
        if rational { "_rational" } else { Default::default() },
    );
    let mut file = match std::fs::File::options()
        .write(true)
        .create(true)
        .truncate(true)
        .open(&path)
    {
        Ok(x) => x,
        Err(e) => panic!("打开文件出错：{}", e),
    };
    file.write_all(b"\xEF\xBB\xBF").unwrap();

    let mut wtr = csv::Writer::from_writer(file);
    let option = EndFiledGacheCalculatorOption::default()
        .with_curren_count(count)
        .with_current_water_level(water_level)
        .with_current_up_count(up_count)
        .with_bonus_at_30(!no_bonus_at_30)
        .with_pity_at_120(!no_pity_at_120)
        .with_bonus_at_each_240(!no_bonus_at_each_240);

    let pb = ProgressBar::new(total_count as u64);
    pb.set_style(
        ProgressStyle::default_bar()
            .template("{spinner:.green} [{elapsed_precise}] [{bar:40.cyan/blue}] {pos}/{len} (ETA: {eta})")
            .unwrap()
            .progress_chars("#>-"),
    );

    if rational {
        match_params!(option, total_count, total_up_count, BigRational, wtr, pb, 1, 2, 3, 4, 5, 6);
    } else {
        match_params!(option, total_count, total_up_count, f64, wtr, pb, 1, 2, 3, 4, 5, 6);
    }

    println!("Saved at {}", path);
}