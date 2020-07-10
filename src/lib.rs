use std::collections::HashMap;

pub fn execute(code: &str) -> String {
    let instructions = parser::parse(code);
    let mut robot = Robot::new();
    robot.collect_functions(&instructions);
    robot.interpret(&instructions);
    let field = robot.get_field();
    field_to_string(field)
}

#[derive (PartialEq, Debug)]
enum Direction {
    Top,
    Bottom,
    Left,
    Right
}

#[derive (PartialEq, Clone, Debug)]
enum Cell {
    Visited,
    NotVisited
}

#[derive (Debug, Clone)]
pub enum Instruction {
    Step(u32),
    TurnLeft(u32),
    TurnRight(u32),
    Block(Vec<Instruction>, u32),
    FunctionDefinition(u32, Vec<Instruction>),
    FunctionInvokation(u32),
    Nop
}

impl Instruction {
    fn set_n(self, n: u32) -> Self {
        use Instruction::*;

        match self {
            Step(_) => Step(n),
            TurnLeft(_) => TurnLeft(n),
            TurnRight(_) => TurnRight(n),
            Block(v, _) => Block(v, n),
            some => some
        }
    }
}

#[derive (Debug)]
struct Position {
    pub x: usize,
    pub y: usize
}

#[derive (Debug)]
struct Robot {
    direction: Direction,
    field: Vec<Vec<Cell>>,
    position: Position,
    functions: HashMap<u32, Vec<Instruction>>
}

impl Robot {
    fn new() -> Robot {
        Robot {
            direction: Direction::Right,
            field: vec![vec![Cell::Visited]],
            position: Position {x: 0, y: 0},
            functions: HashMap::new()
        }
    }

    fn collect_functions(&mut self, instructions: &[Instruction]) {
        use Instruction::*;

        for instruction in instructions.iter() {
            match instruction {
                FunctionDefinition(num, instructions) => {
                    let instructions = instructions.to_vec();
                    self.functions.insert(*num, instructions);
                },
                _ => {}
            }
        }
    }

    fn interpret(&mut self, instructions: &[Instruction]) {
        use Instruction::*;

        for instruction in instructions.iter() {
            match instruction {
                Step(times) =>
                    (0 .. *times).for_each(|_| self.step()),
                TurnLeft(times) =>
                    (0 .. *times).for_each(|_| self.turn_left()),
                TurnRight(times) =>
                    (0 .. *times).for_each(|_| self.turn_right()),
                Block(instructions, times) =>
                    (0 .. *times).for_each(|_| self.interpret(&instructions)),
                FunctionInvokation(num) => {
                    let instructions = self.functions.get(&num).unwrap().to_vec();
                    self.interpret(&instructions);
                },
                FunctionDefinition(_, _) => {},
                Nop => {}
            }
        }
    }

    fn get_field(&mut self) -> &[Vec<Cell>] {
        &self.field
    }

    fn turn_left(&mut self) {
        use Direction::*;

        self.direction = match self.direction {
            Top => Left,
            Left => Bottom,
            Bottom => Right,
            Right => Top
        }
    }

    fn turn_right(&mut self) {
        use Direction::*;

        self.direction = match self.direction {
            Top => Right,
            Right => Bottom,
            Bottom => Left,
            Left => Top
        }
    }

    fn step(&mut self) {
        use Direction::*;

        match self.direction {
            Right => self.step_right(),
            Left => self.step_left(),
            Top => self.step_top(),
            Bottom => self.step_bottom()
        }
    }

    fn step_right(&mut self) {
        if self.position.x + 1 >= self.field[self.position.y].len() {
            for y in 0 .. self.field.len() {
                self.field[y].push(Cell::NotVisited);
            }
        }

        self.position.x += 1;
        self.mark_visited();
    }

    fn step_left(&mut self) {
        if self.position.x == 0 {
            for y in 0 .. self.field.len() {
                self.field[y].insert(0, Cell::NotVisited);
            }
        } else {
            self.position.x -= 1;
        }

        self.mark_visited();
    }

    fn step_bottom(&mut self) {
        if self.position.y + 1 >= self.field.len() {
            self.field.push(
                vec![Cell::NotVisited; self.field[self.position.y].len()]
            )
        }

        self.position.y += 1;
        self.mark_visited();
    }

    fn step_top(&mut self) {
        if self.position.y == 0 {
            self.field.insert(0,
                vec![Cell::NotVisited; self.field[0].len()]
            );
        } else {
            self.position.y -= 1;
        }

        self.mark_visited();
    }

    fn mark_visited(&mut self) {
        self.field[self.position.y][self.position.x] = Cell::Visited;
    }
}

pub mod parser {
    use super::*;
    use super::combinators::*;

    pub fn parse(input: &str) -> Vec<Instruction> {
        let mut instructions = vec![];
        let mut input = input;

        while let Some((next_input, instruction)) = instruction().parse(input) {
            instructions.push(instruction);
            input = next_input;
        }

        instructions
    }

    fn instruction<'a>() -> impl Parser<'a, Instruction> {
        ignore_spaces(
            step_n()
                .or(turn_left_n())
                .or(turn_right_n())
                .or(step())
                .or(turn_left())
                .or(turn_right())
                .or(block_n())
                .or(block())
                .or(function_definition())
                .or(function_invokation())
                .or(line_comment())
                .or(multiline_comment()))
    }

    fn pred<'a, P, F, T>(p: P, f: F)
        -> impl Parser<'a, T>
    where
        P: Parser<'a, T>,
        F: Fn(&T) -> bool
    {
        move |input: &'a str| {
            if let Some((next_input, res)) = p.parse(input) {
                if f(&res) {
                    return Some((next_input, res));
                }
            }

            None
        }
    }

    fn any_char(input: &str) -> ParseRes<char> {
        let mut chars = input.chars();

        match chars.next() {
            Some(ch) => Some((&input[1..], ch)),
            None => None
        }
    }

    fn string<'a>(s: &'a str)
        -> impl Parser<'a, &'a str>
    {
        move |input: &'a str| {
            if input.len() < s.len() {
                return None;
            }

            if &input[0 .. s.len()] == s {
                Some((&input[s.len() ..], s))
            } else {
                None
            }
        }
    }

    fn string_n<'a>(n: usize)
        -> impl Parser<'a, &'a str>
    {
        move |input: &'a str| {
            if input.len() < n {
                None
            } else {
                Some((&input[n ..], &input[0 .. n]))
            }
        }
    }

    fn number(input: &str) -> ParseRes<u32> {
        let mut matched = String::new();
        let mut chars = input.chars();

        match chars.next() {
            Some(ch) => {
                if ch.is_numeric() {
                    matched.push(ch);
                } else {
                    return None;
                }
            },
            None => {
                return None;
            }
        }

        while let Some(ch) = chars.next() {
            if ch.is_numeric() {
                matched.push(ch);
            } else {
                break;
            }
        }

        let next_index = matched.len();
        let num = matched.parse::<u32>().unwrap();
        Some((&input[next_index .. ], num))
    }

    fn space0<'a>()
        -> impl Parser<'a, ()>
    {
        zero_or_more(
            char(' ')
                .or(char('\n'))
            ).map(|_| ())
    }

    fn and1<'a, P1, P2, R1, R2>(p1: P1, p2: P2)
        -> impl Parser<'a, (R1, R2)>
    where
        P1: Parser<'a, R1>,
        P2: Parser<'a, R2>,
    {
        move |input| {
            if let Some((next_input, res1)) = p1.parse(input) {
                if let Some((_, res2)) = p2.parse(input) {
                    return Some((next_input, (res1, res2)));
                }
            }

            None
        }
    }

    fn ignore_spaces<'a, P, T>(p: P)
        -> impl Parser<'a, T>
    where
        P: Parser<'a, T> + 'a,
        T: 'a
    {
        space0()
            .right(p)
            .left(space0())
    }

    fn char<'a>(ch: char)
        -> impl Parser<'a, char>
    {
        move |input: &'a str| {
            match input.chars().next() {
                Some(input_ch) => if input_ch == ch {
                    Some((&input[1..], ch))
                } else {
                    None
                },
                None => None
            }
        }
    }

    fn step<'a>() -> impl Parser<'a, Instruction> {
        char('F')
            .map(|_| Instruction::Step(1))
    }

    fn turn_left<'a>() -> impl Parser<'a, Instruction> {
        char('L')
            .map(|_| Instruction::TurnLeft(1))
    }

    fn turn_right<'a>() -> impl Parser<'a, Instruction> {
        char('R')
            .map(|_| Instruction::TurnRight(1))
    }

    fn block<'a>() -> impl Parser<'a, Instruction> {
        char('(')
            .right(zero_or_more(lazy(instruction)))
            .left(char(')'))
            .map(|instructions| Instruction::Block(instructions, 1))
    }

    fn function_definition<'a>() -> impl Parser<'a, Instruction> {
        char('p')
            .right(number)
            .pair(zero_or_more(lazy(instruction)))
            .left(char('q'))
            .map(|(num, instructions)| Instruction::FunctionDefinition(num, instructions))
    }

    fn function_invokation<'a>() -> impl Parser<'a, Instruction> {
        char('P')
            .right(number)
            .map(|num| Instruction::FunctionInvokation(num))
    }

    fn step_n<'a>() -> impl Parser<'a, Instruction> {
        with_number(step())
    }

    fn turn_left_n<'a>() -> impl Parser<'a, Instruction> {
        with_number(turn_left())
    }

    fn turn_right_n<'a>() -> impl Parser<'a, Instruction> {
        with_number(turn_right())
    }

    fn block_n<'a>() -> impl Parser<'a, Instruction> {
        with_number(block())
    }

    fn with_number<'a, P>(p: P)
        -> impl Parser<'a, Instruction>
    where
        P: Parser<'a, Instruction> + 'a
    {
        p.pair(number).map(|(i, n)| i.set_n(n))
    }

    fn line_comment<'a>() -> impl Parser<'a, Instruction> {
        string("//")
            .pair(
                zero_or_more(
                    pred(any_char, |ch| *ch != '\n')
                )
            )
            .pair(char('\n'))
            .map(|_| Instruction::Nop)
    }

    fn multiline_comment<'a>() -> impl Parser<'a, Instruction> {
        string("/*")
            .pair(
                zero_or_more(
                    and1(
                        any_char,
                        pred(
                            string_n(2),
                            |s| *s != "*/"
                        )
                    )
                )
            )
            .pair(string("*/"))
            .map(|_| Instruction::Nop)
    }

}

pub mod combinators {
    pub type ParseRes<'a, Output> = Option<(&'a str, Output)>;

    pub trait Parser<'a, Output> {
        fn parse(&self, input: &'a str) -> ParseRes<'a, Output>;

        fn map<F, NewOutput>(self, map_fn: F)
            -> BoxedParser<'a, NewOutput>
        where
            Self: Sized + 'a,
            Output: 'a,
            NewOutput: 'a,
            F: Fn(Output) -> NewOutput + 'a
        {
            BoxedParser::new(map(self, map_fn))
        }

        fn pair<P, NewOutput>(self, p: P)
            -> BoxedParser<'a, (Output, NewOutput)>
        where
            P: Parser<'a, NewOutput> + 'a,
            Self: Sized + 'a,
            Output: 'a,
            NewOutput: 'a,
        {
            BoxedParser::new(pair(self, p))
        }

        fn left<P, NewOutput>(self, p: P)
            -> BoxedParser<'a, Output>
        where
            P: Parser<'a, NewOutput> + 'a,
            Self: Sized + 'a,
            Output: 'a,
            NewOutput: 'a,
        {
            BoxedParser::new(left(self, p))
        }

        fn right<P, NewOutput>(self, p: P)
            -> BoxedParser<'a, NewOutput>
        where
            P: Parser<'a, NewOutput> + 'a,
            Self: Sized + 'a,
            Output: 'a,
            NewOutput: 'a,
        {
            BoxedParser::new(right(self, p))
        }

        fn or<P>(self, parser: P)
            -> BoxedParser<'a, Output>
        where
            Self: Sized + 'a,
            Output: 'a,
            P: 'a,
            P: Parser<'a, Output>
        {
            BoxedParser::new(or(self, parser))
        }

        fn debug(self, name: &'a str)
            -> BoxedParser<'a, Output>
        where
            Self: Sized + 'a,
            Output: std::fmt::Debug + 'a
        {
            BoxedParser::new(debug(name, self))
        }
    }

    impl<'a, F, Output> Parser<'a, Output> for F
    where
        F: Fn(&'a str) -> ParseRes<Output>,
    {
        fn parse(&self, input: &'a str) -> ParseRes<'a, Output> {
            self(input)
        }
    }

    pub struct BoxedParser<'a, Output> {
        parser: Box<dyn Parser<'a, Output> + 'a>,
    }

    impl<'a, Output> BoxedParser<'a, Output> {
        pub fn new<P>(parser: P) -> Self
        where
            P: Parser<'a, Output> + 'a
        {
            BoxedParser {
                parser: Box::new(parser)
            }
        }
    }

    impl<'a, Output> Parser<'a, Output> for BoxedParser<'a, Output> {
        fn parse(&self, input: &'a str) -> ParseRes<'a, Output> {
            self.parser.parse(input)
        }
    }

    pub struct LazyParser<'a, Output> {
        constructor: Box<dyn Fn() -> BoxedParser<'a, Output> + 'a>
    }

    impl<'a, Output> LazyParser<'a, Output> {
        pub fn new<F>(constructor: F) -> Self
        where
            F: Fn() -> BoxedParser<'a, Output> + 'a
        {
            LazyParser { constructor: Box::new(constructor) }
        }
    }

    impl<'a, Output> Parser<'a, Output> for LazyParser<'a, Output> {
        fn parse(&self, input: &'a str) -> ParseRes<'a, Output> {
            (self.constructor)().parse(input)
        }
    }

    pub fn map<'a, P, F, A, B>(parser: P, map_fn: F)
        -> impl Parser<'a, B>
    where
        P: Parser<'a, A>,
        F: Fn(A) -> B
    {
        move |input|
            parser.parse(input)
                .map(|(next_input, res)| (next_input, map_fn(res)))
    }

    fn debug<'a, P, T>(name: &'a str, p: P)
        -> impl Parser<'a, T>
    where
        P: Parser<'a, T>,
        T: std::fmt::Debug
    {
        move |input| {
            let res = p.parse(input);
            if res.is_none() {
                println!("Parser {} failed", name);
                println!("Parser {} input:\n{}", name, input);
                println!("End {} input", name);
            } else {
                println!("Parser {} ok", name);
                println!("Parser {} input:\n{}", name, input);
                println!("Parser {} result:\n{:?}", name, res.as_ref().unwrap());
            }
            res
        }
    }

    pub fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2)
        -> impl Parser<'a, (R1, R2)>
    where
        P1: Parser<'a, R1>,
        P2: Parser<'a, R2>
    {
        move |input| match parser1.parse(input) {
            Some((next_input, result1)) => match parser2.parse(next_input) {
                Some((final_input, result2)) => Some((final_input, (result1, result2))),
                None => None,
            },
            None => None,
        }
    }

    pub fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R1>
    where
        P1: Parser<'a, R1> + 'a,
        P2: Parser<'a, R2> + 'a,
        R1: 'a,
        R2: 'a
    {
        parser1
            .pair(parser2)
            .map(|(left, _)| left)
    }

    pub fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
    where
        P1: Parser<'a, R1> + 'a,
        P2: Parser<'a, R2> + 'a,
        R1: 'a,
        R2: 'a
    {
        parser1
            .pair(parser2)
            .map(|(_, right)| right)
    }

    pub fn zero_or_more<'a, P, T>(parser: P)
        -> impl Parser<'a, Vec<T>>
    where
        P: Parser<'a, T>
    {
        move |mut input| {
            let mut res = vec![];

            while let Some((next_input, val)) = parser.parse(input) {
                input = next_input;
                res.push(val);
            }

            Some((input, res))
        }
    }

    pub fn or<'a, P1, P2, T>(parser1: P1, parser2: P2)
        -> impl Parser<'a, T>
    where
        P1: Parser<'a, T>,
        P2: Parser<'a, T>,
    {
        move |input| match parser1.parse(input) {
            some @ Some(_) => some,
            None => parser2.parse(input)
        }
    }

    pub fn lazy<'a, P, T, F>(f: F)
        -> LazyParser<'a, T>
    where
        P: Parser<'a, T> + 'a,
        F: Fn() -> P + 'a,
    {
        LazyParser::new(move || BoxedParser::new(f()))
    }

}

fn field_to_string(field: &[Vec<Cell>]) -> String {
    use Cell::*;

    field.iter()
        .map(|row| row.iter()
            .map(|cell|
                match *cell {
                    Visited => '*',
                    NotVisited => ' '
            })
            .collect::<String>()
        )
        .collect::<Vec<String>>()
        .join("\r\n")
}


#[cfg(test)]
macro_rules! assert_equal {
  ($actual:expr, $expected:expr $(,)*) => {{
    let actual = $actual;
    let expected = $expected;
    assert_eq!(actual, expected, "\ngot:\n{}\n\nexpected:\n{}\n", actual, expected);
  }};
}

#[test]
fn examples_in_description() {
  assert_equal!(
    execute("LF5(RF3)(RF3R)F7"),
    "    ****\r\n    *  *\r\n    *  *\r\n********\r\n    *   \r\n    *   ",
  );
  assert_equal!(
    execute("(L(F5(RF3))(((R(F3R)F7))))"),
    "    ****\r\n    *  *\r\n    *  *\r\n********\r\n    *   \r\n    *   ",
  );
  assert_equal!(
    execute("F4L(F4RF4RF4LF4L)2F4RF4RF4"),
    "    *****   *****   *****\r\n    *   *   *   *   *   *\r\n    *   *   *   *   *   *\r\n    *   *   *   *   *   *\r\n*****   *****   *****   *",
  );
  assert_equal!(
    execute("F4L((F4R)2(F4L)2)2(F4R)2F4"),
    "    *****   *****   *****\r\n    *   *   *   *   *   *\r\n    *   *   *   *   *   *\r\n    *   *   *   *   *   *\r\n*****   *****   *****   *",
  );

  assert_equal!(
    execute("p0(F2LF2R)2qP0"),
    "    *\r\n    *\r\n  ***\r\n  *  \r\n***  "
  );

  assert_eq!(
    execute("p312(F2LF2R)2qP312"),
    "    *\r\n    *\r\n  ***\r\n  *  \r\n***  "
  );

  assert_equal!(execute("P0p0(F2LF2R)2q"), "    *\r\n    *\r\n  ***\r\n  *  \r\n***  ");
  assert_equal!(execute("P312p312(F2LF2R)2q"), "    *\r\n    *\r\n  ***\r\n  *  \r\n***  ");
  assert_equal!(execute("F3P0Lp0(F2LF2R)2qF2"), "       *\r\n       *\r\n       *\r\n       *\r\n     ***\r\n     *  \r\n******  ");
  assert_equal!(execute("(P0)2p0F2LF2RqP0"), "      *\r\n      *\r\n    ***\r\n    *  \r\n  ***  \r\n  *    \r\n***    ");

    assert_equal!(
        execute(r#"/*
  RoboScript Ultimatum (RSU)
  A simple and comprehensive code example
*/

// Define a new pattern with identifier n = 0
p0
  // The commands below causes the MyRobot to move
  // in a short snake-like path upwards if executed
  (
    F2 L // Go forwards two steps and then turn left
  )2 (
    F2 R // Go forwards two steps and then turn right
  )2
q

// Execute the snake-like pattern twice to generate
// a longer snake-like pattern
(
  P0
)2
"#),
    "*  \r\n*  \r\n***\r\n  *\r\n***\r\n*  \r\n***\r\n  *\r\n***"
    );
}
