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
- Constants: Store predefined values to be used in expressions.
- Functions: Define and use custom functions using lambda expressions, along with built-in functions in expressions.
  - Conditional Function Attributes: Define conditions that parameters must meet before being passed to a function.
  - Preprocess Function Attributes: Define preprocesses that are applied to a parameter before being passed to a function.
- Collections: Expressions that are groups containing other functions
  - Vectors: Vectors can contain any number of elements in a set order, allowing for non-unique elements.
  - Sets: Sets can contain any number of unique elements in an indeterminate order.
- Indexers: An expression to get an element from another expression

## Installation

### From NuGet Using Visual Studio

1. Create or Open your solution/project
1. In the Solution Explorer, find the "Dependencies" item in your project
1. Right click "Dependencies" and select "Manage NuGet packages..."
1. In the "Browse" tab, search for "CalcExpr.NET"
1. Install the "CalcExpr.NET" package

### From NuGet Using dotnet CLI

1. Launch the terminal of your choice
1. Navigate to where your csproj file is located
1. Enter `dotnet add package CalcExpr.NET`

### From GitHub Releases

1. Go to the Releases section of this repo
1. Find the version you want
1. Download the CalcExpr.dll file from that release
1. Add the file as a reference to your project

## Usage

Here's a simple example demonstrating how to use CalcExpr.NET:

```csharp
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

Alternatively, a Tokenizer can be provided to specify tokenizing behavior or new parsing rules can be supplied to to modify parsing behavior.

## Contributing

We welcome contributions to CalcExpr.NET! Whether it's bug fixes, new features, or improvements to the documentation, your help is greatly appreciated. More information can be found in this file repository's [CONTRIBUTING.md](https://github.com/andrewk17111/CalcExpr.NET/blob/dev/CONTRIBUTING.md) file.

### How to Contribute

1. **Fork the Repository**: Create a fork of this repository.
1. **Create a Branch**: Make a new branch for your changes based on the `dev` branch.
1. **Make Your Changes**: Implement your changes and commit them with clear and descriptive commit messages.
1. **Submit a Pull Request**: Open a pull request to the `dev` branch of the main repository, detailing the changes you made and why.

### Code of Conduct

We adhere to a Code of Conduct to ensure a welcoming and inclusive environment for everyone. Please read and follow our [Code of Conduct](https://github.com/andrewk17111/CalcExpr.NET/blob/dev/CONTRIBUTING.md) before contributing.

## License

CalcExpr.NET is released under the GPL-3.0 License.

## Branches

### Release/X.X

Release branch following Major.Minor. Upon release, patches will be pushed to these branches and tagged.
New NuGet releases will be tagged on these branches.

### Dev

Development branch. This branch is what pull requests are targetted to.
