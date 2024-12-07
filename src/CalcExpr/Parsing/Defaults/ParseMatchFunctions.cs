﻿using CalcExpr.Attributes;
using CalcExpr.Expressions;
using CalcExpr.Expressions.Collections;
using CalcExpr.Expressions.Components;
using CalcExpr.Expressions.Functions;
using CalcExpr.Expressions.Terminals;
using CalcExpr.Extensions;
using CalcExpr.Parsing.Rules;
using CalcExpr.Parsing.Tokens;
using CalcExpr.Tokenization.Tokens;
using System.Collections.Immutable;
using System.Reflection;

namespace CalcExpr.Parsing.Defaults;

internal static class ParseMatchFunctions
{
    internal static IEnumerableExpression ParseMatchCollection(ImmutableArray<IToken> _, TokenMatch match, Parser parser)
    {
        ImmutableArray<IToken> condensed = match[1..^1].Condense(Brackets.Square | Brackets.Curly);
        IEnumerable<IExpression> enumerable = condensed.Split(',').Select(element => parser.Parse(element.Uncondense()));

        return (match.First() as OpenBracketToken)!.BracketType == Bracket.Square
            ? new Vector(enumerable)
            : new Set(enumerable);
    }

    internal static FunctionCall ParseMatchFunctionCall(ImmutableArray<IToken> _, TokenMatch match, Parser parser)
    {
        string functionName = match.First().Value;
        ImmutableArray<IToken> condensedArgs = match[2..^1].Condense();

        IEnumerable<IExpression> args = condensedArgs
            .Split(',')
            .Select(arg => parser.Parse(arg.Uncondense()));

        return new FunctionCall(functionName, args);
    }

    internal static LambdaFunction ParseMatchLambdaFunction(ImmutableArray<IToken> input, TokenMatch match, Parser parser)
    {
        ImmutableArray<IToken> parameterTokens = match.First() is OpenBracketToken { BracketType: Bracket.Parenthesis }
            ? match[1..^3]
            : match[..^2];
        ImmutableArray<IToken> condensedParameters = parameterTokens.Condense(Brackets.Square);
        IEnumerable<Parameter> parameters = condensedParameters.Split(',')
            .Select(p =>
            {
                ImmutableArray<IToken>[] attributes = p.First() is CondensedToken attribute
                    ? attribute.Tokens[1..^1].Split(',')
                    : [];

                return new Parameter(p.Last().Value, attributes);
            }).ToArray();

        return new LambdaFunction(parameters, parser.Parse(input[match.Length..]));
    }

    internal static Parentheses ParseMatchParentheses(ImmutableArray<IToken> _, TokenMatch match, Parser parser)
        => new Parentheses(parser.Parse(match[..]));

    internal static IExpression ParseMatchWithParentheses(ImmutableArray<IToken> input, TokenMatch _, Parser parser)
    {
        ImmutableArray<IToken> condensed = input.Condense(Brackets.Parenthesis);

        foreach (IParserRule rule in parser.Grammar)
        {
            TokenMatch? subMatch = rule.Match(condensed, parser.Grammar);

            if (subMatch is not null)
            {
                IExpression? expression = rule.Parse(input, new TokenMatch(subMatch.Match.ToImmutableArray().Uncondense(),
                    condensed.UncondenseIndex(subMatch.Index)), parser);

                if (expression is not null)
                    return expression;
            }
        }

        throw new Exception($"The input was not in the correct format: '{input.JoinTokens()}'");
    }

    internal static AssignmentOperator ParseMatchAssignmentOperator(ImmutableArray<IToken> input, TokenMatch match, Parser parser)
        => new AssignmentOperator((parser.Parse(input[..match.Index]) as Variable)!, parser.Parse(input[(match.Index + match.Length)..]));

    internal static BinaryOperator ParseMatchBinaryOperator(ImmutableArray<IToken> input, TokenMatch match, Parser parser)
        => new BinaryOperator(match.Value, parser.Parse(input[..match.Index]), parser.Parse(input[(match.Index + match.Length)..]));

    internal static PrefixOperator ParseMatchPrefix(ImmutableArray<IToken> input, TokenMatch match, Parser parser)
        => new PrefixOperator(match.Value, parser.Parse(input[(match.Index + match.Length)..]));

    internal static PostfixOperator ParseMatchPostfix(ImmutableArray<IToken> input, TokenMatch match, Parser parser)
        => new PostfixOperator(match.Value, parser.Parse(input[..match.Index]));

    internal static Indexer ParseMatchIndexer(ImmutableArray<IToken> input, TokenMatch match, Parser parser)
        => new Indexer(parser.Parse(input[..match.Index]), parser.Parse(match[1..^1]));

    internal static Undefined ParseMatchUndefined(IToken match, Parser __)
        => match.Value switch
        {
            "undefined" => Undefined.UNDEFINED,
            "dne" => Undefined.DNE,
            _ => throw new Exception($"The input was not in the correct format: '{match.Value}'")
        };

    internal static Logical ParseMatchLogical(IToken match, Parser __)
        => match.Value switch
        {
            "true" => Logical.TRUE,
            "false" => Logical.FALSE,
            _ => throw new Exception($"The input was not in the correct format: '{match.Value}'")
        };

    internal static Infinity ParseMatchInfinity(IToken match, Parser __)
        => match.Value switch
        {
            "∞" => Infinity.POSITIVE,
            "infinity" => Infinity.POSITIVE_INFINITY,
            "inf" => Infinity.POSITIVE_INF,
            _ => throw new Exception($"The input was not in the correct format: '{match.Value}'")
        };

    internal static Constant ParseMatchConstant(IToken match, Parser __)
        => new Constant(match.Value);

    internal static Variable ParseMatchVariable(WordToken match, Parser __)
        => new Variable(match.Value);

    internal static Number ParseMatchNumber(NumberToken match, Parser __)
        => new Number(match.ParsedValue);
}
