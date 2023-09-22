using CalcExpr.Expressions;
using System.Text.RegularExpressions;

namespace CalcExpr.Parsing.Rules;

public class RegexRule : Rule
{
    public readonly string RegularExpression;
    public readonly RegexRuleOptions Options;
    
    public RegexOptions RegularOptions
        => (RegexOptions)((int)Options & 0x0000FFFF);

    /// <summary>
    /// A rule to be used to parse a <see cref="string"/> into an <see cref="IExpression"/>.
    /// </summary>
    /// <param name="name">The name of the <see cref="RegexRule"/>.</param>
    /// <param name="regex">The regex <see cref="string"/> to match an expression <see cref="string"/> to.</param>
    /// <param name="options">The options for the regular expression.</param>
    /// <param name="parse">
    /// The function to use to parse a <see cref="string"/> into an <see cref="IExpression"/>.
    /// </param>
    public RegexRule(string name, string regex, RegexOptions options,
        Func<string, Token, Parser, IExpression> parse)
        : this(name, regex, (RegexRuleOptions)options, parse)
    { }

    /// <summary>
    /// A rule to be used to parse a <see cref="string"/> into an <see cref="IExpression"/>.
    /// </summary>
    /// <param name="name">The name of the <see cref="RegexRule"/>.</param>
    /// <param name="regex">The regex <see cref="string"/> to match an expression <see cref="string"/> to.</param>
    /// <param name="options">
    /// The options for the regular expression along with additional options finding a match.
    /// </param>
    /// <param name="parse">
    /// The function to use to parse a <see cref="string"/> into an <see cref="IExpression"/>.
    /// </param>
    public RegexRule(string name, string regex, RegexRuleOptions options,
        Func<string, Token, Parser, IExpression> parse)
        : base(name, parse, null!)
    {
        RegularExpression = regex;
        Options = options;
    }

    public override Token? Match(string input, IEnumerable<Rule> rules)
        => FindMatch(input, RegularExpression, Options);

    /// <summary>
    /// Find a matching substring in the input <see cref="string"/> using the provided regular expression along with the
    /// additional <see cref="RegexRuleOptions"/>.
    /// </summary>
    /// <param name="input">The <see cref="string"/> to find the match in.</param>
    /// <param name="regex">The regular expression to use to find a match.</param>
    /// <param name="options">
    /// The options for the regular expression along with additional option for matching found in
    /// <see cref="RegexOptions"/>.
    /// </param>
    /// <returns>
    /// A new <see cref="Token"/> containing the value of the matching <see cref="string"/> and the index of where it
    /// was found; <see langword="null"/> if no match was found.
    /// </returns>
    protected static Token? FindMatch(string input, string regex, RegexRuleOptions options)
    {
        string trimmed_input = input;
        int offset = 0;

        if (options.HasFlag(RegexRuleOptions.TrimLeft))
        {
            trimmed_input = trimmed_input.TrimStart();
            offset = input.Length - trimmed_input.Length;
        }

        if (options.HasFlag(RegexRuleOptions.TrimRight))
            trimmed_input = trimmed_input.TrimEnd();

        Match match = Regex.Match(trimmed_input, regex, (RegexOptions)((int)options & 0x0000FFFF));

        if (match.Success)
        {
            if (options.HasFlag(RegexRuleOptions.Left))
            {
                bool left = false;

                foreach (Group group in match.Groups)
                {
                    if (group.Index == 0)
                    {
                        left = true;
                        break;
                    }
                }

                if (!left)
                    return null;
            }

            if (options.HasFlag(RegexRuleOptions.Right))
            {
                bool right = false;

                foreach (Group group in match.Groups)
                {
                    if (group.Index + group.Length == trimmed_input.Length)
                    {
                        right = true;
                        break;
                    }
                }

                if (!right)
                    return null;
            }

            return new Token(match.Value, match.Index + offset);
        }

        return null;
    }

    public override bool Equals(object? obj)
        => obj is not null && obj is RegexRule a && RegularExpression == a.RegularExpression;

    public override int GetHashCode()
        => RegularExpression.GetHashCode();

    public static bool operator ==(RegexRule a, RegexRule b)
        => a.Equals(b);

    public static bool operator !=(RegexRule a, RegexRule b)
        => !a.Equals(b);
}

/// <summary>
/// Provides enumerated values to use to set regular expression options and additional matching options.
/// </summary>
[Flags]
public enum RegexRuleOptions
{
    /// <summary>
    /// Specifies that no options are set. For more information about the default behavior of the regular expression
    /// engine, see the "Default Options" section in the Regular Expression Options article.
    /// </summary>
    None = 0x000,
    /// <summary>
    /// Specifies case-insensitive matching. For more information, see the "Case-Insensitive Matching" section in the
    /// Regular Expression Options article.
    /// </summary>
    IgnoreCase = 0x001,
    /// <summary>
    /// Multiline mode. Changes the meaning of ^ and $ so they match at the beginning and end, respectively, of any
    /// line, and not just the beginning and end of the entire string. For more information, see the "Multiline Mode"
    /// section in the Regular Expression Options article.
    /// </summary>
    Multiline = 0x002,
    /// <summary>
    /// Specifies that the only valid captures are explicitly named or numbered groups of the form (?<name>...). This
    /// allows unnamed parentheses to act as noncapturing groups without the syntactic clumsiness of the expression
    /// (?:...). For more information, see the "Explicit Captures Only" section in the Regular Expression Options
    /// article.
    /// </summary>
    ExplicitCapture = 0x004,
    /// <summary>
    /// Specifies that the regular expression is compiled to MSIL code, instead of being interpreted. Compiled regular
    /// expressions maximize run-time performance at the expense of initialization time. This value should not be
    /// assigned to the <see cref="RegexCompilationInfo.Options"/> property when calling the
    /// <see cref="Regex.CompileToAssembly(RegexCompilationInfo[], System.Reflection.AssemblyName)"/> method. For more
    /// information, see the "Compiled Regular Expressions" section in the Regular Expression Options article.
    /// </summary>
    Compiled = 0x008,
    /// <summary>
    /// Specifies single-line mode. Changes the meaning of the dot (.) so it matches every character (instead of every
    /// character except \n). For more information, see the "Single-line Mode" section in the Regular Expression Options
    /// article.
    /// </summary>
    Singleline = 0x010,
    /// <summary>
    /// Eliminates unescaped white space from the pattern and enables comments marked with #. However, this value does
    /// not affect or eliminate white space in character classes, numeric quantifiers, or tokens that mark the beginning
    /// of individual regular expression language elements. For more information, see the "Ignore White Space" section
    /// of the Regular Expression Options article.
    /// </summary>
    IgnorePatternWhitespace = 0x020,
    /// <summary>
    /// Specifies that the search will be from right to left instead of from left to right. For more information, see
    /// the "Right-to-Left Mode" section in the Regular Expression Options article.
    /// </summary>
    RightToLeft = 0x040,
    /// <summary>
    /// Enables ECMAScript-compliant behavior for the expression. This value can be used only in conjunction with the
    /// <see cref="RegexOptions.IgnoreCase"/>, <see cref="RegexOptions.Multiline"/>, and
    /// <see cref="RegexOptions.Compiled"/> values. The use of this value with any other values results in an exception.
    /// For more information on the System.Text.RegularExpressions.RegexOptions.ECMAScript option, see the "ECMAScript
    /// Matching Behavior" section in the Regular Expression Options article.
    /// </summary>
    ECMAScript = 0x100,
    /// <summary>
    /// Specifies that cultural differences in language is ignored. For more information, see the "Comparison Using the
    /// Invariant Culture" section in the Regular Expression Options article.
    /// </summary>
    CultureInvariant = 0x200,
    /// <summary>
    /// Specifies that the matching substring (including any lookbehinds) must be at the beginning of the
    /// <see cref="string"/>.
    /// </summary>
    Left = 0x0001_0000,
    /// <summary>
    /// Specifies that the matching substring (including any lookaheads) must be at the end of the <see cref="string"/>.
    /// </summary>
    Right = 0x0002_0000,
    /// <summary>
    /// Specifies that the matching substring (including any lookbehinds or lookaheads) must be at the beginning and end
    /// of the <see cref="string"/>, encompassing the whole <see cref="string"/>. (Is the same as setting both the
    /// <see cref="Left"/> and <see cref="Right"/> flags.)
    /// </summary>
    Only = 0x0003_0000,
    /// <summary>
    /// When used in a <see cref="NestedRegexRule"/>, white space selectors will be added before and after the
    /// referenced regex.
    /// </summary>
    PadReferences = 0x0004_0000,
    /// <summary>
    /// Removes all of the leading whitespace from the beginning of the <see cref="string"/>.
    /// </summary>
    TrimLeft = 0x0008_0000,
    /// <summary>
    /// Removes all of the trailing whitespace from the end of the <see cref="string"/>.
    /// </summary>
    TrimRight = 0x0010_0000,
    /// <summary>
    /// Removes all of the leading and trailing whitespace from the beginning and end of the <see cref="string"/>. (Is
    /// the same as setting both the <see cref="TrimLeft"/> and <see cref="TrimRight"/> flags.)
    /// </summary>
    Trim = 0x0018_0000
}
