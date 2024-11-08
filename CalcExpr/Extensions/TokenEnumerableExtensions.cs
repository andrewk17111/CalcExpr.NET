﻿using CalcExpr.Tokenization.Tokens;
using System.Text;

namespace CalcExpr.Extensions;

internal static class TokenEnumerableExtensions
{
    internal static string JoinTokens(this IEnumerable<IToken> tokens)
    {
        int offset = tokens.FirstOrDefault()?.Index ?? 0;
        StringBuilder builder = new();

        foreach (IToken token in tokens)
        {
            builder.Append(string.Join("", new int[token.Index - offset - builder.Length].Select(_ => ' ')));
            builder.Append(token.Value);
        }

        return builder.ToString();
    }

    internal static List<IToken>[] Split(this List<IToken> tokens, string separator)
    {
        List<List<IToken>> result = [];
        List<IToken> subset = [];

        foreach (IToken token in tokens)
        {
            if (token.Value == separator)
            {
                result.Add(subset);
                subset = [];
            }
            else
            {
                subset.Add(token);
            }
        }

        if (subset.Count > 0)
            result.Add(subset);

        return [.. result];
    }

    internal static List<IToken>[] Split(this List<IToken> tokens, char separator)
        => Split(tokens, separator.ToString());

    internal static List<IToken>[] Split(this List<IToken> tokens, params string[] separator)
    {
        List<List<IToken>> result = [];
        List<IToken> subset = [];

        for (int i = 0; i < tokens.Count - separator.Length; i++)
        {
            if (tokens[i..(i + separator.Length)].Select(t => t.Value).SequenceEqual(separator))
            {
                result.Add(subset);
                subset = [];
                i += separator.Length - 1;
            }
            else
            {
                subset.Add(tokens[i]);
            }
        }

        if (subset.Count > 0)
            result.Add(subset);

        return [.. result];
    }
}
