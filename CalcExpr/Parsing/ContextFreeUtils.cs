using CalcExpr.Exceptions;
using System.Text;
using System.Text.RegularExpressions;

namespace CalcExpr.Parsing;

public static class ContextFreeUtils
{
    private static (string, string) GetBracketStrings(Brackets brackets = Brackets.All)
    {
        string open_brackets = (brackets.HasFlag(Brackets.Parenthesis) ? "(" : "") +
            (brackets.HasFlag(Brackets.Square) ? "[" : "") +
            (brackets.HasFlag(Brackets.Curly) ? "{" : "") +
            (brackets.HasFlag(Brackets.Angle) ? "<" : "");
        string close_brackets = (brackets.HasFlag(Brackets.Parenthesis) ? ")" : "") +
            (brackets.HasFlag(Brackets.Square) ? "]" : "") +
            (brackets.HasFlag(Brackets.Curly) ? "}" : "") +
            (brackets.HasFlag(Brackets.Angle) ? ">" : "");

        return (open_brackets, close_brackets);
    }

    /// <summary>
    /// Tokenizes the input string by replacing all bracketed expressions with a token.
    /// </summary>
    /// <param name="input">The input string to tokenize.</param>
    /// <param name="tokens">The tokens that were created from the input string.</param>
    /// <param name="brackets">Type of brackets to tokenize.</param>
    /// <returns>The tokenized string.</returns>
    /// <exception cref="UnbalancedParenthesesException">
    /// Thrown when the input string has unbalanced brackets.
    /// </exception>
    public static string TokenizeInput(this string input, out Token[] tokens, Brackets brackets = Brackets.All)
    {
        if (brackets == Brackets.None)
        {
            tokens = [];
            return input;
        }

        input = Regex.Replace(input, brackets.HasFlag(Brackets.Square) ? @"[\\]" : @"[\[\]\\]",
            match => @$"\{match.Value}");

        (string open_brackets, string close_brackets) = GetBracketStrings(brackets);
        List<Token> toks = [];
        StringBuilder output = new StringBuilder();
        int start = -1;
        int offset = 0;
        int local_offset = 0;
        Stack<char> depth = [];

        for (int i = 0; i < input.Length; i++)
        {
            char current = input[i];

            if (current == '\\')
            {
                if (start < 0)
                {
                    output.Append($"{current}{input[++i]}");
                }
                else
                {
                    i++;
                    local_offset++;
                }

                offset++;
                continue;
            }

            if (open_brackets.Contains(current))
            {
                if (start == -1)
                {
                    start = i;
                    local_offset = 0;
                }

                depth.Push(current);
            }
            
            int close_index = close_brackets.IndexOf(current);

            if (close_index != -1)
            {
                if (start < 0 || open_brackets[close_index] != depth.Pop())
                {
                    throw new UnbalancedParenthesesException(input, i);
                }
                else if (depth.Count == 0)
                {
                    output.Append($"[{toks.Count}]");
                    toks.Add(new Token(input[start..(i + 1)], start - (offset - local_offset)));
                    start = -1;
                    depth.Clear();
                }
            }
            else
            {
                if (start < 0)
                    output.Append(current);
                else if (i == input.Length - 1)
                    throw new UnbalancedParenthesesException(input, i);
            }
        }

        tokens = [.. toks];
        return output.ToString();
    }

    /// <summary>
    /// Tokenizes the input string by replacing all bracketed expressions with a token.
    /// </summary>
    /// <param name="input">The input string to tokenize.</param>
    /// <param name="tokens">The tokens that were created from the input string.</param>
    /// <param name="tokenized">The tokenized string.</param>
    /// <param name="brackets">Type of brackets to tokenize.</param>
    /// <returns>
    /// <see langword="true"/> if the input string was tokenized successfully, <see langword="false"/> otherwise.
    /// </returns>"
    public static bool TryTokenizeInput(this string input, out Token[] tokens, out string? tokenized,
        Brackets brackets = Brackets.All)
    {
        try
        {
            tokenized = input.TokenizeInput(out tokens, brackets);
            return true;
        }
        catch
        {
            tokens = [];
            tokenized = null;
            return false;
        }
    }

    /// <summary>
    /// Checks if the input string has balanced brackets.
    /// </summary>
    /// <param name="input">The input string to check.</param>
    /// <param name="brackets">Type of brackets to check.</param>
    /// <returns>
    /// <see langword="true"/> if the input string has balanced brackets, <see langword="false"/> otherwise.
    /// </returns>
    public static bool CheckBalancedBrackets(this string input, Brackets brackets = Brackets.All)
    {
        if (brackets == Brackets.None)
            return true;

        input = Regex.Replace(input, brackets.HasFlag(Brackets.Square) ? @"[\\]" : @"[\[\]\\]",
            match => @$"\{match.Value}");

        (string open_brackets, string close_brackets) = GetBracketStrings(brackets);
        Dictionary<char, int> open_counts = open_brackets.ToDictionary(c => c, c => 0);
        Dictionary<char, int> close_counts = close_brackets.ToDictionary(c => c, c => 0);

        for (int i = 0; i < input.Length; i++)
        {
            char current = input[i];

            if (current == '\\')
            {
                i++;
            }
            else if (open_brackets.Contains(current))
            {
                open_counts[current]++;
            }
            else if (close_brackets.Contains(current))
            {
                if (open_counts[open_brackets[close_brackets.IndexOf(current)]] == 0)
                    return false;

                close_counts[current]++;
            }
        }

        return !open_brackets.Select((c, i) => open_counts[c] == close_counts[close_brackets[i]])
            .Any(x => !x);
    }

    /// <summary>
    /// Converts the index of the tokenized string to the index of the original string.
    /// </summary>
    /// <param name="index">The index to detokenize.</param>
    /// <param name="tokenized_string">The tokenized string.</param>
    /// <param name="tokens">The tokens that were created from the input string.</param>
    /// <returns>The index of the original string.</returns>
    public static int DetokenizeIndex(int index, string tokenized_string, Token[] tokens)
    {
        string[] matches =
            [.. Regex.Matches(tokenized_string[..index], @"((?<=^|([^\\]([\\][\\])*))\[\d+\])|(\\[\[\]\\])")
                .Select(m => m.Value)];

        foreach (string match in matches)
            if (match.StartsWith('\\'))
                index -= 1;
            else
                index += tokens[Convert.ToInt32(match[1..^1])].Length - 3;

        return index;
    }

    /// <summary>
    /// Converts the indexes of the tokenized string to the indexes of the original string.
    /// </summary>
    /// <param name="indexes">The indexes to detokenize.</param>
    /// <param name="tokenized_string">The tokenized string.</param>
    /// <param name="tokens">The tokens that were created from the input string.</param>
    /// <param name="sort">Whether to sort the indexes before detokenizing.</param>
    /// <returns>The indexes of the original string.</returns>
    public static IEnumerable<int> DetokenizeIndexes(IEnumerable<int> indexes, string tokenized_string,
        Token[] tokens, bool sort = true)
    {
        if (!sort)
        {
            foreach (int idx in indexes.Select(i => DetokenizeIndex(i, tokenized_string, tokens)))
                yield return idx;

            yield break;
        }

        if (!indexes.Any())
            yield break;

        int[] sorted_indexes = [.. indexes.Order()];
        Match[] matches = [.. Regex.Matches(tokenized_string[..sorted_indexes.Last()],
                @"((?<=^|([^\\]([\\][\\])*))\[\d+\])|(\\[\[\]\\])")
            .Where(m => !m.Value.StartsWith('\\'))];
        int match_index = -1;

        if (matches.Length != 0)
        {
            Match? match = null;

            for (int i = 0; i < sorted_indexes.Length; i++)
            {
                while (match_index < matches.Length - 1 && matches[match_index + 1].Index < sorted_indexes[i])
                    match = matches[++match_index];

                if (match is null)
                    yield return sorted_indexes[i] - Regex.Matches(tokenized_string[..sorted_indexes[i]], @"\\.").Count;
                else
                    yield return tokens[Convert.ToInt32(match.Value[1..^1])].Index +
                        tokens[Convert.ToInt32(match.Value[1..^1])].Length - 3 + (sorted_indexes[i] - match.Index);
            }
        }
        else
        {
            int prev_index = 0;
            int offset = 0;

            for (int i = 0; i < sorted_indexes.Length; i++)
            {
                offset += Regex.Matches(tokenized_string[prev_index..sorted_indexes[i]], @"\\.").Count;
                yield return sorted_indexes[i] - offset;
                prev_index = sorted_indexes[i];
            }
        }
    }
}

public enum Brackets
{
    None = 0,
    Parenthesis = 1,
    Square = 2,
    Curly = 4,
    Angle = 8,
    All = 0xFF,
}
