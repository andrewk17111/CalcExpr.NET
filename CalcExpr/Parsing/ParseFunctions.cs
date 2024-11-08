using CalcExpr.Expressions;
using CalcExpr.Expressions.Collections;
using CalcExpr.Extensions;
using CalcExpr.Tokenization.Tokens;

namespace CalcExpr.Parsing;

internal static class ParseFunctions
{
    internal static IExpression? ParseCollection(List<IToken> input, Parser parser)
    {
        if (input.Count >= 2 && input.First() is OpenBracketToken open && input.Last() is CloseBracketToken close &&
            open.BracketType == close.BracketType && (Bracket.Square | Bracket.Curly).HasFlag(open.BracketType) &&
            input[1..^1].TryCondense(out List<IToken>? condensed, Brackets.Square | Brackets.Curly))
        {
            IEnumerable<IExpression> enumerable = condensed!.Split(',').Select(e => parser.Parse(e.Uncondense()));

            return open.BracketType == Bracket.Square
                ? new Vector(enumerable)
                : new Set(enumerable);
        }

        return null;
    }

    internal static IExpression? ParseFunctionCall(List<IToken> input, Parser parser)
    {
        if (input.Count >= 3 && input.First() is WordToken functionName && input[1] is OpenBracketToken { BracketType: Bracket.Parenthesis } &&
            input.Last() is CloseBracketToken { BracketType: Bracket.Parenthesis })
        {
            List<IToken> args = input[2..^1];

            if (args.Count > 0)
            {
                List<IToken> condensedArgs = args.Condense(~Brackets.Angle);
                IEnumerable<IExpression> arguments = condensedArgs.Split(',').Select(arg => parser.Parse(arg.Uncondense()));

                return new FunctionCall(functionName.Value, arguments);
            }
        }

        return null;        
    }
}
