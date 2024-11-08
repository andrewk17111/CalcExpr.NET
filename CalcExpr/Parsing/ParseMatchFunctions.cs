using CalcExpr.Attributes;
using CalcExpr.Expressions;
using CalcExpr.Expressions.Collections;
using CalcExpr.Expressions.Components;
using CalcExpr.Expressions.Functions;
using CalcExpr.Expressions.Terminals;
using CalcExpr.Extensions;
using CalcExpr.Parsing.Rules;
using CalcExpr.Parsing.Tokens;
using CalcExpr.Tokenization.Tokens;
using System.Reflection;
using System.Text.RegularExpressions;

namespace CalcExpr.Parsing;

internal static class ParseMatchFunctions
{
    internal static IEnumerableExpression ParseMatchCollection(List<IToken> _, TokenMatch match, Parser parser)
    {
        List<IToken> condensed = ContextFreeUtils.Condense(match[1..^1], Brackets.Square | Brackets.Curly);
        IEnumerable<IExpression> enumerable = condensed.Split(',').Select(element => parser.Parse(element.Uncondense()));

        return (match.First() as OpenBracketToken)!.BracketType == Bracket.Square
            ? new Vector(enumerable)
            : new Set(enumerable);
    }

    internal static FunctionCall ParseMatchFunctionCall(List<IToken> _, TokenMatch match, Parser parser)
    {
        string functionName = match.First().Value;
        List<IToken> condensedArgs = match[2..^1].Condense();

        IEnumerable<IExpression> args = condensedArgs
            .Split(',')
            .Select(arg => parser.Parse(arg.Uncondense()));

        return new FunctionCall(functionName, args);
    }

    internal static LambdaFunction ParseMatchLambdaFunction(List<IToken> input, TokenMatch match, Parser parser)
    {
        List<IToken> parameterTokens = match.First() is OpenBracketToken { BracketType: Bracket.Parenthesis }
            ? match[1..^3]
            : match[..^2];
        List<IToken> condensedParameters = parameterTokens.Condense(Brackets.Square);
        IEnumerable<Parameter> parameters = condensedParameters.Split(',')
            .Select(p =>
            {
                List<IToken>[] attributes = p.First() is CondensedToken attribute
                    ? attribute.Tokens[1..^1].Split(',')
                    : [];

                return new Parameter(p.Last().Value, attributes);
            }).ToArray();

        return new LambdaFunction(parameters, parser.Parse(input[match.Length..]));
    }

    internal static Parentheses ParseMatchParentheses(List<IToken> _, TokenMatch match, Parser parser)
        => new Parentheses(parser.Parse(match[..]));

    internal static IExpression ParseMatchWithParentheses(List<IToken> input, TokenMatch _, Parser parser)
    {
        List<IToken> condensed = input.Condense(Brackets.Parenthesis);

        foreach (IParserRule rule in parser.Grammar)
        {
            if (rule.GetType().GetCustomAttribute<ReferenceRuleAttribute>() is not null)
                continue;

            TokenMatch? subMatch = rule.Match(condensed, parser.Grammar);

            if (subMatch is not null)
            {
                IExpression? expression = rule.Parse(input, new TokenMatch(subMatch.Match.ToList().Uncondense(),
                    condensed.UncondenseIndex(subMatch.Index)), parser);

                if (expression is not null)
                    return expression;
            }
        }

        throw new Exception($"The input was not in the correct format: '{input.JoinTokens()}'");
    }

    internal static AssignmentOperator ParseMatchAssignmentOperator(List<IToken> input, TokenMatch match, Parser parser)
        => new AssignmentOperator((parser.Parse(input[..match.Index]) as Variable)!, parser.Parse(input[(match.Index + match.Length)..]));

    internal static BinaryOperator ParseMatchBinaryOperator(List<IToken> input, TokenMatch match, Parser parser)
        => new BinaryOperator(match.Value, parser.Parse(input[..match.Index]), parser.Parse(input[(match.Index + match.Length)..]));

    internal static PrefixOperator ParseMatchPrefix(List<IToken> input, TokenMatch match, Parser parser)
        => new PrefixOperator(match.Value, parser.Parse(input[(match.Index + match.Length)..]));

    internal static PostfixOperator ParseMatchPostfix(List<IToken> input, TokenMatch match, Parser parser)
        => new PostfixOperator(match.Value, parser.Parse(input[..match.Index]));

    internal static Indexer ParseMatchIndexer(List<IToken> input, TokenMatch match, Parser parser)
        => new Indexer(parser.Parse(input[..match.Index]), parser.Parse(match[1..^1]));

    internal static Undefined ParseMatchUndefined(List<IToken> _, TokenMatch match, Parser __)
        => match.Value switch
        {
            "undefined" => Undefined.UNDEFINED,
            "dne" => Undefined.DNE,
            _ => throw new Exception($"The input was not in the correct format: '{match.Value}'")
        };

    internal static Logical ParseMatchLogical(List<IToken> _, TokenMatch match, Parser __)
        => new Logical(Boolean.Parse(match.Value));

    internal static Infinity ParseMatchInfinity(List<IToken> _, TokenMatch match, Parser __)
        => new Infinity(match.Value);

    internal static Constant ParseMatchConstant(List<IToken> _, TokenMatch match, Parser __)
        => new Constant(match.Value);

    internal static Variable ParseMatchVariable(List<IToken> _, TokenMatch match, Parser __)
        => new Variable(match.Value);

    internal static Number ParseMatchNumber(List<IToken> _, TokenMatch match, Parser __)
        => new Number(((NumberToken)match.Match.Single()).ParsedValue);
}
