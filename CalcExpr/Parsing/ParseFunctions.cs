using CalcExpr.Expressions;
using System.Text.RegularExpressions;
using static CalcExpr.Parsing.MatchFunctions;

namespace CalcExpr.Parsing;

internal static class ParseFunctions
{
    internal static IExpression? ParseFunctionCall(string input, Parser parser)
    {
        input = input.Trim();

        if (String.IsNullOrWhiteSpace(input))
            return null;

        Match function_name = Regex.Match(input, @"^([A-Za-zΑ-Ωα-ω]+(_[A-Za-zΑ-Ωα-ω0-9]+)*)");

        if (function_name.Success)
        {
            string args = input[function_name.Length..].TrimStart();

            if (args[0] == '(' && args[^1] == ')')
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
