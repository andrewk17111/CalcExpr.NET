using CalcExpr.Expressions;
using CalcExpr.Expressions.Collections;
using CalcExpr.Tokenization.Tokens;
using System.Text.RegularExpressions;

namespace CalcExpr.Parsing;

internal static class ParseFunctions
{
    internal static IExpression? ParseCollection(string input, Parser parser)
    {
        Match match = Regex.Match(input, @"(?<=^\s*)[\[\{].*?[\]\}](?=\s*$)");

        if (match.Success && match.Value[^1] - match.Value[0] == 2 &&
            ContextFreeUtils.TryTokenizeInput(match.Value[1..^1], out Token[] tokens, out string? tokenized,
            Brackets.Square | Brackets.Curly))
        {
            IEnumerable<IExpression> enumerable = tokenized!.Split(',')
                .Select(e => parser.Parse(Regex.Replace(e, @"(?<!(^|[^\\])\\(\\\\)*)\[\d+\]", m => m.Value[1..^1])));

            return match.Value[0] == '['
                ? new Vector(enumerable)
                : new Set(enumerable);
        }

        return null;
    }

    internal static IExpression? ParseFunctionCall(string input, Parser parser)
    {
        input = input.Trim();

        if (String.IsNullOrWhiteSpace(input))
            return null;

        Match function_name = Regex.Match(input, @"^([A-Za-zΑ-Ωα-ω]+(_[A-Za-zΑ-Ωα-ω0-9]+)*)");

        if (function_name.Success)
        {
            string args = input[function_name.Length..].TrimStart();

            if (!String.IsNullOrWhiteSpace(args) && args[0] == '(' && args[^1] == ')')
            {
                string tokenized_args = args[1..^1].TokenizeInput(out Token[] tokens);
                string[] split_args = tokenized_args
                    .Split(",", StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries)
                    .Select(arg => !arg.Contains('[')
                        ? arg
                        : Regex.Replace(arg, @"\[\d+\]", match => tokens[Convert.ToInt32(match.Value[1..^1])]))
                    .ToArray();

                return new FunctionCall(function_name.Value, split_args.Select(parser.Parse));
            }
        }

        return null;        
    }
}
