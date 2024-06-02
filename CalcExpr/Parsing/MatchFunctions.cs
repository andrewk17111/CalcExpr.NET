using CalcExpr.Parsing.Rules;
using System.Text.RegularExpressions;

namespace CalcExpr.Parsing;

internal static class MatchFunctions
{
    internal static Token? MatchCollection(string input, IEnumerable<IRule> _)
    {
        Match match = Regex.Match(input, @"(?<=^\s*)[\[\{].*?[\]\}](?=\s*$)");

        if (match.Success && match.Value[^1] - match.Value[0] == 2)
        {
            try
            {
                if (ContextFreeUtils.CheckBalancedBrackets(match.Value[1..^1], Brackets.Square | Brackets.Curly))
                    return match;
            }
            catch
            {
                throw;
            }
        }

        return null;
    }

    internal static Token? MatchFunctionCall(string input, IEnumerable<IRule> rules)
    {
        input = input.Trim();

        if (String.IsNullOrWhiteSpace(input))
            return null;

        Match function_name = Regex.Match(input, @"^([A-Za-zΑ-Ωα-ω]+(_[A-Za-zΑ-Ωα-ω0-9]+)*)");

        if (function_name.Success)
        {
            Token? parentheses = MatchParentheses(input[function_name.Length..], rules);

            if (parentheses is not null)
                return new Token(
                    input[..(function_name.Length + parentheses.Value.Index + parentheses.Value.Length + 1)],
                    0);
        }

        return null;
    }

    internal static Token? MatchParentheses(string input, IEnumerable<IRule> _)
    {
        input = input.Trim();

        if (String.IsNullOrWhiteSpace(input))
            return null;

        if (input[0] == '(' && input[^1] == ')')
        {
            int depth = 0;

            for (int i = 1; i < input.Length - 1; i++)
            {
                char current = input[i];

                if (current == '(')
                {
                    depth++;
                }
                else if (current == ')')
                {
                    if (depth == 0)
                        return null;

                    depth--;
                }
            }

            if (depth == 0)
                return new Token(input[1..^1], 1);
        }

        return null;
    }

    internal static Token? MatchIndexer(string input, IEnumerable<IRule> _)
    {
        ContextFreeUtils.TokenizeInput(input, out Token[] tokens, Brackets.Square);

        if (tokens.Length > 0)
        {
            Token match = tokens.Last();

            if (!match[1..^1].Contains(','))
                return match;
        }

        return null;
    }
}
