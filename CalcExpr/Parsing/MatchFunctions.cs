using CalcExpr.Parsing.Rules;
using CalcExpr.Parsing.Tokens;
using CalcExpr.Tokenization.Tokens;

namespace CalcExpr.Parsing;

internal static class MatchFunctions
{
    internal static TokenMatch? MatchCollection(List<IToken> input, IEnumerable<IParserRule> _)
    {
        if (input.Count >= 2 && input.First() is OpenBracketToken open && input.Last() is CloseBracketToken close &&
            open.BracketType == close.BracketType && (Bracket.Square | Bracket.Curly).HasFlag(open.BracketType))
        {
            return new TokenMatch(input, 0);
        }

        return null;
    }

    internal static TokenMatch? MatchFunctionCall(List<IToken> input, IEnumerable<IParserRule> _)
    {
        if (input.Count >= 3 && input.First() is WordToken && input[1] is OpenBracketToken { BracketType: Bracket.Parenthesis } &&
            input.Last() is CloseBracketToken { BracketType: Bracket.Parenthesis })
        {
            return new TokenMatch(input, 0);
        }

        return null;
    }

    internal static TokenMatch? MatchParentheses(List<IToken> input, IEnumerable<IParserRule> _)
    {
        if (input.Count >= 2 && input.First() is OpenBracketToken { BracketType: Bracket.Parenthesis } &&
            input.Last() is CloseBracketToken { BracketType: Bracket.Parenthesis })
        {
            int depth = 0;

            for (int i = 1; i < input.Count - 1; i++)
            {
                IToken current = input[i];

                if (current is OpenBracketToken { BracketType: Bracket.Parenthesis })
                {
                    depth++;
                }
                else if (current is CloseBracketToken { BracketType: Bracket.Parenthesis })
                {
                    if (depth == 0)
                        return null;

                    depth--;
                }
            }

            if (depth == 0)
                return new TokenMatch(input[1..^1], 1);
        }

        return null;
    }

    internal static TokenMatch? MatchIndexer(List<IToken> input, IEnumerable<IParserRule> _)
    {
        IEnumerable<CondensedToken> tokens = ContextFreeUtils.Condense(input, Brackets.Square)
            .Where(token => token is CondensedToken condensed && (condensed.Tokens.First() as OpenBracketToken)?.BracketType == Bracket.Square)
            .Cast<CondensedToken>();

        if (tokens.Any())
        {
            CondensedToken match = tokens.Last();

            if (match.Tokens.Count == 3 && match.Tokens[1] is NumberToken)
                return new TokenMatch(match.Tokens, match.Index);
        }

        return null;
    }
}
