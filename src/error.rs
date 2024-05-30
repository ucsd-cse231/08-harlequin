use codespan_reporting::term;
use ess::span::ByteSpan;
use std::fmt::{self, Display};
use std::{fs::File, io::Read};

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

#[derive(Debug)]
pub struct Error {
    span: ByteSpan,
    msg: String,
}

pub trait Span: Clone {
    fn span(&self) -> ByteSpan;
}

impl Span for ByteSpan {
    fn span(&self) -> ByteSpan {
        *self
    }
}

impl Error {
    pub fn new(span: ByteSpan, msg: String) -> Self {
        Error { span, msg }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.msg)
    }
}

pub fn read_file(file: &str) -> String {
    let mut file = File::open(file).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    contents
}

pub fn render_errors(file_name: &str, errors: Vec<Error>) {
    let mut files = SimpleFiles::new();
    let contents = read_file(file_name);
    let file_id = files.add(file_name, contents);
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();

    for error in errors {
        let diagnostic = Diagnostic::error()
            .with_message(error.msg)
            .with_labels(vec![Label::primary(file_id, error.span.0..error.span.1)]);

        let _ = term::emit(&mut writer.lock(), &config, &files, &diagnostic);
    }
}

// pub fn demo() {
//     let mut files = SimpleFiles::new();

//     let file_id = files.add(
//         "FizzBuzz.fun",
//         unindent::unindent(
//             r#"
//             module FizzBuzz where

//             fizz₁ : Nat → String
//             fizz₁ num = case (mod num 5) (mod num 3) of
//                 0 0 => "FizzBuzz"
//                 0 _ => "Fizz"
//                 _ 0 => "Buzz"
//                 _ _ => num

//             fizz₂ : Nat → String
//             fizz₂ num =
//                 case (mod num 5) (mod num 3) of
//                     0 0 => "FizzBuzz"
//                     0 _ => "Fizz"
//                     _ 0 => "Buzz"
//                     _ _ => num
//         "#,
//         ),
//     );

//     // We normally recommend creating a custom diagnostic data type for your
//     // application, and then converting that to `codespan-reporting`'s diagnostic
//     // type, but for the sake of this example we construct it directly.

//     let diagnostic = Diagnostic::error()
//         .with_message("`case` clauses have incompatible types")
//         .with_code("E0308")
//         .with_labels(vec![
//             Label::primary(file_id, 328..331).with_message("expected `String`, found `Nat`"),
//             Label::secondary(file_id, 211..331)
//                 .with_message("`case` clauses have incompatible types"),
//             Label::secondary(file_id, 258..268)
//                 .with_message("this is found to be of type `String`"),
//             Label::secondary(file_id, 284..290)
//                 .with_message("this is found to be of type `String`"),
//             Label::secondary(file_id, 306..312)
//                 .with_message("this is found to be of type `String`"),
//             Label::secondary(file_id, 186..192).with_message("expected type `String` found here"),
//         ])
//         .with_notes(vec![unindent::unindent(
//             "
//             expected type `String`
//                 found type `Nat`
//         ",
//         )]);

//     // We now set up the writer and configuration, and then finally render the
//     // diagnostic to standard error.

//     let writer = StandardStream::stderr(ColorChoice::Always);
//     let config = codespan_reporting::term::Config::default();

//     let _ = term::emit(&mut writer.lock(), &config, &files, &diagnostic);
// }
