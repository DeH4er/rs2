pub fn execute(code: &str) -> String {
    let instructions = parser::parse(code);
    let mut robot = Robot::new();
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

#[derive (Debug)]
pub enum Instruction {
    Step(u32),
    TurnLeft(u32),
    TurnRight(u32),
    Block(Vec<Instruction>, u32)
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
    position: Position
}

impl Robot {
    fn new() -> Robot {
        Robot {
            direction: Direction::Right,
            field: vec![vec![Cell::Visited]],
            position: Position {x: 0, y: 0}
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

    fn step<'a>() ->
        impl Parser<'a, Instruction>
    {
        map(char('F'), |_| Instruction::Step(1))
    }

    fn turn_left<'a>() ->
        impl Parser<'a, Instruction>
    {
        map(char('L'), |_| Instruction::TurnLeft(1))
    }

    fn turn_right<'a>() ->
        impl Parser<'a, Instruction>
    {
        map(char('R'), |_| Instruction::TurnRight(1))
    }

    fn step_n<'a>() ->
        impl Parser<'a, Instruction>
    {
        map(
            pair(char('F'), number),
            |(_, n)| Instruction::Step(n)
        )
    }

    fn turn_left_n<'a>() ->
        impl Parser<'a, Instruction>
    {
        map(
            pair(char('L'), number),
            |(_, n)| Instruction::TurnLeft(n)
        )
    }

    fn turn_right_n<'a>() ->
        impl Parser<'a, Instruction>
    {
            map(
                pair(char('R'), number),
                |(_, n)| Instruction::TurnRight(n)
            )
    }

    fn block<'a>() ->
        impl Parser<'a, Instruction>
    {
        left(
            right(
                char('('),
                zero_or_more(lazy(instruction)),
            ),
            char(')')
        )
        .map(|instructions| Instruction::Block(instructions, 1))
    }

    fn block_n<'a>() ->
        impl Parser<'a, Instruction>
    {
        pair(
            left(
                right(
                    char('('),
                    zero_or_more(lazy(instruction)),
                ),
                char(')')
            ),
            number
        )
        .map(|(instructions, n)| Instruction::Block(instructions, n))
    }

    fn instruction<'a>() ->
        impl Parser<'a, Instruction>
    {
        step_n()
            .or(turn_left_n())
            .or(turn_right_n())
            .or(step())
            .or(turn_left())
            .or(turn_right())
            .or(block_n())
            .or(block())
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

    pub fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
    where
        P: Parser<'a, A>,
        F: Fn(A) -> B
    {
        move |input|
            parser.parse(input)
                .map(|(next_input, res)| (next_input, map_fn(res)))
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
        P1: Parser<'a, R1>,
        P2: Parser<'a, R2>,
    {
        map(pair(parser1, parser2), |(left, _right)| left)
    }

    pub fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
    where
        P1: Parser<'a, R1>,
        P2: Parser<'a, R2>,
    {
        map(pair(parser1, parser2), |(_left, right)| right)
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
}
