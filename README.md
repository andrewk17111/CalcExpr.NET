# CalcExpr.NET
![Lines of Code](https://tokei.rs/b1/github/andrewk17111/CalcExpr.NET?style=flat-square)
[![License](https://img.shields.io/github/license/andrewk17111/CalcExpr.NET?style=flat-square)](https://github.com/andrewk17111/CalcExpr.NET/blob/main/LICENSE)
[![Open Issues](https://img.shields.io/github/issues/andrewk17111/CalcExpr.NET?style=flat-square)](https://github.com/andrewk17111/CalcExpr.NET/issues)
[![GitHub Issues or Pull Requests](https://img.shields.io/github/issues-pr/andrewk17111/CalcExpr.NET?style=flat-square)](https://github.com/andrewk17111/CalcExpr.NET/pulls)

A .NET 8 math expression parser and calculator library

CalcExpr.NET is a versatile and efficient math expression parsing library developed in C# targeting .NET 8. This library allows you to parse strings containin math expressions into expression trees and then evaluated to calculate the resulting value.

## Features
- Expression Parsing: Parse and tokenize mathematical expressions provided as strings.
- Unary Operations: Perfoms unary operations supporting prefixes and postfixes.
- Binary Operations: Perform arithmetic operations such as addition, subtraction, multiplication, division, integer division, and modulus.
- Parentheses Support: Handle nested expressions using parentheses for accurate calculations.
- Variables: Store values in variables and use them in expressions.
- Functions: Define and use custom functions using lambda expressions, along with built-in functions in expressions.
  - Conditional Function Attributes: Define conditions that parameters must meet before being passed to a function.
  - Preprocess Function Attributes: Define preprocesses that are applied to a parameter before being passed to a function.
- Collections: Expressions that are groups containing other functions
  - Vectors: Vectors can contain any number of elements in a set order, allowing for non-unique elements.
  - Sets: Sets can contain any number of unique elements in an indeterminate order.
- Indexers: An expression to get an element from another expression

## Installation
Because this project is still under active development, you can install this by either downloading this repository and building the project, or by downloading the latest experimental build from the section of this repository.

## Usage
Here's a simple example demonstrating how to use CalcExpr.NET:

``` csharp
using CalcExpr.Parsing;

public class Program
{
    public static void Main()
    {
        // Create a new math parser instance.
        Parser parser = new Parser();

        // Evaluate an expression.
        IExpression result = parser.Parse("2 * -10 + 3").Evaluate();

        Console.WriteLine("Result: " + result);
        // Displays Result: -17
    }
}
```

## License
CalcExpr.NET is released under the GPL-3.0 License.
