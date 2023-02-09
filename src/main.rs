use float_cmp::ApproxEq;
use nom::bytes::complete::{is_not, tag, take_until};
use nom::{multi::*, sequence::*};
use regex::Regex;
use std::collections::HashMap;
use std::io::Write;
use std::io::{BufRead, BufReader};
use std::{env, fs::File, io, path::Path, str::FromStr};

#[derive(Debug)]
struct Renderer {
    pub lines: Vec<String>,
    pub variables: HashMap<String, String>,
    pub labels: HashMap<String, usize>,
    pub index: usize,
}

impl Renderer {
    fn new() -> Renderer {
        Renderer {
            lines: Vec::new(),
            variables: HashMap::new(),
            labels: HashMap::new(),
            index: 0,
        }
    }

    fn processfile(&mut self, file: File) {
        let reader = BufReader::new(file);

        for (index, curline) in reader.lines().enumerate() {
            let text = curline.unwrap();
            self.lines.push(text.clone());

            if text.is_empty() {
                continue;
            }

            match &text[0..1] {
                ":" => {
                    self.labels.insert(text[1..].to_string(), index);
                }
                "@" => {
                    match self.tokenize(self.lines[index].clone(), "=") {
                        Ok((l, _)) => self.variables.insert(l[1..].to_string(), String::from("0")),
                        Err(_) => continue,
                    };
                }
                _ => continue,
            }
        }
    }

    fn process_variables(&self, text: &String) -> String {
        let mut s = text.clone();

        for item in parse_variables(text).iter() {
            if !text.is_empty() {
                let var = match self.variables.get(&item[..]) {
                    Some(v) => v,
                    None => panic!(
                        "Variable Missing at line {}. It must be created before the block using it.",
                        self.index
                    ),
                };
                s = s.replace(&format!("@{}", &item[..]), var);
            }
        }
        s
    }

    fn process_expression(&self, text: String) -> bool {
        let (left, mid, right) = self.get_expression(text);
        let mut isnan = false;

        let lvalue: f64 = match tinyexpr::interp(&left[..]) {
            Ok(v) => v as f64,
            Err(_) => {
                isnan = true;
                0.0_f64
            }
        };

        let rvalue: f64 = match tinyexpr::interp(&right[..]) {
            Ok(v) => v as f64,
            Err(_) => {
                isnan = true;
                0.0_f64
            }
        };

        match &mid[..] {
            "==" => {
                if isnan {
                    left == right
                } else {
                    lvalue.approx_eq(
                        rvalue,
                        float_cmp::F64Margin {
                            ulps: 16,
                            epsilon: 0.0,
                        },
                    )
                }
            }
            "!=" => {
                if isnan {
                    left != right
                } else {
                    !lvalue.approx_eq(
                        rvalue,
                        float_cmp::F64Margin {
                            ulps: 16,
                            epsilon: 0.0,
                        },
                    )
                }
            }
            "<=" => {
                if isnan {
                    panic!("strings cant be compared with <=, line {}", self.index + 1)
                } else {
                    lvalue <= rvalue
                }
            }
            ">=" => {
                if isnan {
                    panic!("strings cant be compared with >=, line {}", self.index + 1)
                } else {
                    lvalue >= rvalue
                }
            }
            "<" => {
                if isnan {
                    panic!("strings cant be compared with <, line {}", self.index + 1)
                } else {
                    lvalue < rvalue
                }
            }
            ">" => {
                if isnan {
                    panic!("strings cant be compared with >, line {}", self.index + 1)
                } else {
                    lvalue > rvalue
                }
            }
            _ => panic!("No expression pattern found. line {}", self.index + 1),
        }
    }

    fn get_expression(&self, text: String) -> (String, String, String) {
        let re = Regex::new(r"!=|==|<=|>=|<|>").unwrap();
        let mut mid = String::new();

        let mut part = re.captures_iter(&text[..]);
        mid.push_str(&part.next().unwrap()[0]);

        let arr: Vec<&str> = text.split(&mid[..]).collect();

        if arr.len() != 2 {
            panic!(
                "Expressions must containa a left side, right side and a operator. Line {}",
                self.index
            );
        }

        (String::from(arr[0]), mid, String::from(arr[1]))
    }

    fn tokenize(&self, line: String, pat: &str) -> Result<(String, String), String> {
        let arr: Vec<&str> = line.split(pat).collect();

        if arr.len() != 2 {
            return Err(format!(
                "The Token {} contained {} but should have only 2 at line {}.
            It should be seperated by {}",
                line,
                arr.len(),
                self.index,
                pat,
            ));
        }

        let mut iter = arr.iter();
        Ok((
            String::from(*iter.next().expect("expected 2 names, got 0")),
            String::from(*iter.next().expect("expected 2 names, got 1")),
        ))
    }

    fn iftokenize(
        &self,
        line: String,
        pat: &str,
    ) -> Result<(usize, String, String, String), String> {
        let arr: Vec<&str> = line.split(pat).collect();

        if arr.len() < 2 || arr.len() > 3 {
            return Err(format!(
                "The Token {} contained {} but should have 2 or 3 parts at line {}.
            It should be seperated by {}",
                line,
                arr.len(),
                self.index + 1,
                pat,
            ));
        }

        let mut iter = arr.iter();
        Ok((
            arr.len(),
            String::from(*iter.next().expect("expected 2 names, got 0")),
            String::from(*iter.next().expect("expected 2 names, got 1")),
            String::from(*iter.next().unwrap_or(&"")),
        ))
    }

    fn input_wait(&mut self) {
        println!("\nPress Enter to Continue.");
        read_line();
        self.clear_screen();
    }

    fn clear_screen(&mut self) {
        clear();
        self.index += 1;
    }

    fn printmove(&mut self, s: &String) {
        println!("{}", self.process_variables(s));
        self.index += 1;
    }

    fn process_input(&mut self) {
        let (left, right) = self.tokenize(self.lines[self.index].clone(), ":").unwrap();
        let mut ret;

        if !self.variables.contains_key(&right[1..]) {
            panic!(
                "A Variable must be initalized outside of a Input statement before it can be used.
            The Variable {} on line {} is not Initalized yet.",
                &right[1..],
                self.index + 1
            );
        }

        match &left[1..2] {
            "i" => loop {
                println!("\n{}", &left[2..]);

                ret = read_line();

                if ret.chars().any(char::is_alphabetic) {
                    println!("You may only enter in a Number. Please try again.");
                    continue;
                } else {
                    break;
                }
            },
            "s" => {
                println!("\n{}", &left[2..]);
                ret = read_line();
            }
            _ => panic!(
                "Missing a i or s for input type at line {}. Example: ^i hows many?",
                self.index + 1
            ),
        }

        *self.variables.get_mut(&right[1..]).unwrap() = ret;
        self.index += 1;
    }

    fn process_questions(&mut self) {
        let mut gotos: Vec<String> = Vec::new();
        let mut q = 0;

        while !self.lines[self.index].is_empty() && &self.lines[self.index][0..1] == "?" {
            let (left, right) = self.tokenize(self.lines[self.index].clone(), ":").unwrap();
            gotos.push(right.replace("#", ""));
            println!("{}. {}", q + 1, &left[1..]);
            q += 1;
            self.index += 1;
        }

        let mut input: usize = 0;
        let mut ret;

        while input < 1 || input > q {
            println!("Enter a number from 1 to {}", q);
            ret = read_line();

            if ret.chars().any(char::is_alphabetic) || ret.is_empty() {
                println!("You must use a number");
                continue;
            }

            input = match i32::from_str(&ret[..]) {
                Ok(i) => i as usize,
                Err(_) => {
                    println!("You must use a number");
                    continue;
                }
            };
        }

        let label = gotos.get(input - 1).unwrap();
        match self.labels.get(label) {
            Some(v) => self.index = *v,
            None => {
                panic!(
                    "Goto {} Missing. Found on Question near line {}.",
                    label, self.index
                );
            }
        };
    }

    fn process_variable(&mut self, opt: Option<String>) {
        let text = match &opt {
            None => self.lines[self.index].clone(),
            Some(s) => s.clone(),
        };

        match self.tokenize(text, "=") {
            Ok((l, r)) => {
                let p = self.process_variables(&r);
                match tinyexpr::interp(&p[..]) {
                    //update as variable
                    Ok(v) => *self.variables.get_mut(&l[1..]).unwrap() = v.to_string(),
                    //no calulations done becuase its a string so process as string.
                    Err(_) => *self.variables.get_mut(&l[1..]).unwrap() = p.clone(),
                };

                self.index += 1;
            }
            Err(_) => match &opt {
                None => self.printmove(&self.lines[self.index].clone()),
                Some(_) => panic!(
                    "A Variable must be initalized before it can be used. Error on line {}.",
                    self.index + 1
                ),
            },
        };
    }

    fn process_goto(&mut self, opt: Option<String>) {
        let text = match opt {
            None => self.lines[self.index].clone(),
            Some(s) => s,
        };

        let label = text.replace("#", "").replace(":", "");
        match self.labels.get(&label) {
            Some(v) => self.index = *v,
            None => panic!("Goto {} Missing. line {}", label, self.index + 1),
        };
    }

    fn process_if(&mut self) {
        let (count, left, mid, right) = self
            .iftokenize(self.lines[self.index].clone(), ":")
            .unwrap();
        let exp = self.process_variables(&left[1..left.len()].to_string());
        let mut cond = mid.trim();

        if !self.process_expression(exp) {
            match count {
                3 => cond = right.trim(),
                _ => {
                    self.index += 1;
                    return;
                }
            }
        }

        match &cond[0..1] {
            "#" => self.process_goto(Some(cond.to_string())),
            "@" => self.process_variable(Some(cond.to_string())),
            "\"" => {
                let s = cond[1..cond.len()].to_string();

                if !s.ends_with('"') {
                    panic!("if you start with \" you must End with \" to be able to print. Error on line {}", self.index + 1);
                }

                self.printmove(&s.trim_end_matches('"').to_string())
            }
            _ => self.printmove(&cond.to_string()),
        }
    }
}

fn clear() {
    std::io::stdout().write_all(b"\x1b[2J\x1b[1;1H").unwrap()
}

fn read_line() -> String {
    let mut rv = String::new();
    io::stdin().read_line(&mut rv).unwrap();
    rv.replace("\r\n", "").replace("\n", "")
}

fn parse_variables(line: &String) -> Vec<String> {
    let arr: nom::IResult<&str, Vec<&str>> = many0(preceded(
        take_until("@"),
        preceded(tag("@"), is_not(" \0+-<>=().!#:;^/\\@[]")),
    ))(&line[..]);

    match &arr {
        Ok(v) => {
            let mut ret = Vec::new();

            for item in v.1.iter() {
                ret.insert(0, (*item).to_string())
            }

            ret
        }
        _ => Vec::new(),
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let path = Path::new(&args[1]);
    let display = path.display();
    let mut story = Renderer::new();

    let file = match File::open(&path) {
        Err(why) => panic!("couldn't open {}: {}", display, why),
        Ok(file) => file,
    };

    story.processfile(file);

    while story.index < story.lines.len() {
        let text = &story.lines[story.index];

        if text.is_empty() {
            story.index += 1;
            continue;
        }

        match &text[0..1] {
            "\n" | "\r" | ":" | "*" => story.index += 1,
            "|" => {
                println!();
                story.index += 1;
            }
            "#" => story.process_goto(None),
            "!" => story.process_if(),
            "@" => story.process_variable(None),
            "?" => story.process_questions(),
            "^" => story.process_input(),
            "~" => story.input_wait(),
            "`" => story.clear_screen(),
            _ => story.printmove(&story.lines[story.index].clone()),
        }
    }
}