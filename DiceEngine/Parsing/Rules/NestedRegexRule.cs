using DiceEngine.Exceptions;
using DiceEngine.Expressions;
using System.Text.RegularExpressions;

namespace DiceEngine.Parsing.Rules;

/// <summary>
/// A rule to be used to parse a <see cref="string"/> into an <see cref="IExpression"/>.
/// </summary>
/// <param name="name">The name of the <see cref="NestedRegexRule"/>.</param>
/// <param name="regex_template">
/// The regex template <see cref="string"/> to match an expression <see cref="string"/> to.
/// </param>
/// <param name="options">
/// The options for the regular expression along with additional options finding a match.
/// </param>
/// <param name="parse">
/// The function to use to parse a <see cref="string"/> into an <see cref="IExpression"/>.
/// </param>
public class NestedRegexRule(string name, string regex_template, RegexRuleOptions options,
    Func<string, Token, Parser, IExpression> parse) : RegexRule(name, null!, options, parse)
{
    private string? _regex = null;

    public readonly string RegularExpressionTemplate = regex_template;

    public new string? RegularExpression
        => _regex;

    /// <summary>
    /// A rule to be used to parse a <see cref="string"/> into an <see cref="IExpression"/>.
    /// </summary>
    /// <param name="name">The name of the <see cref="NestedRegexRule"/>.</param>
    /// <param name="regex">
    /// The regex template <see cref="string"/> to match an expression <see cref="string"/> to.
    /// </param>
    /// <param name="options">The options for the regular expression.</param>
    /// <param name="parse">
    /// The function to use to parse a <see cref="string"/> into an <see cref="IExpression"/>.
    /// </param>
    public NestedRegexRule(string name, string regex_template, RegexOptions options,
        Func<string, Token, Parser, IExpression> parse)
        : this(name, regex_template, (RegexRuleOptions)options, parse)
    { }

    public override Token? Match(string input, IEnumerable<Rule> rules)
    {
        if (RegularExpression is null)
            Build(rules);

        return FindMatch(input, RegularExpression!, Options);
    }

    public void Build(IEnumerable<Rule> rules)
    {
        _regex = ReplaceRules(RegularExpressionTemplate, rules, Options);
    }

    private static string ReplaceRules(string regular_expression, IEnumerable<Rule> rules, RegexRuleOptions options)
    {
        List<string> matches = Regex.Matches(regular_expression, @"(?<=(^|[^\\](\\\\)*){)\w+(?=})")
            .Select(m => m.Value)
            .Distinct()
            .ToList();

        if (matches.Count > 0)
        {
            string regex = regular_expression;
            Dictionary<string, int> rule_names = rules.Select((r, i) => new KeyValuePair<string, int>(r.Name, i))
                .ToDictionary(kvp => kvp.Key, kvp => kvp.Value);

            while (matches.Count > 0)
            {
                string match = matches.First();
                string sub_regex;

                if (rule_names.TryGetValue(match, out int idx))
                {
                    if (rules.ElementAt(idx) is NestedRegexRule nrr)
                    {
                        if (nrr.RegularExpression is null)
                            nrr.Build(rules);

                        sub_regex = ReplaceRules(nrr.RegularExpression!, rules, options);
                    }
                    else if (rules.ElementAt(idx) is RegexRule rr)
                    {
                        sub_regex = rr.RegularExpression;
                    }
                    else
                    {
                        throw new NonRegexRuleException(match, rules.ElementAt(idx).GetType());
                    }

                    rule_names.Remove(match);
                    matches.RemoveAt(0);
                }
                else
                {
                    matches.RemoveAt(0);
                    continue;
                }

                if (options.HasFlag(RegexRuleOptions.PadReferences))
                    sub_regex = $@"(\s*{sub_regex}\s*)";

                regex = Regex.Replace(regex, @$"(?<=^|[^\\](\\\\)*){{{match}}}", sub_regex);
            }

            return regex;
        }

        return regular_expression;
    }

    public override bool Equals(object? obj)
        => obj is not null && obj is RegexRule a && RegularExpression == a.RegularExpression;

    public override int GetHashCode()
        => RegularExpressionTemplate.GetHashCode();

    public static bool operator ==(NestedRegexRule a, NestedRegexRule b)
        => a.Equals(b);

    public static bool operator !=(NestedRegexRule a, NestedRegexRule b)
        => !a.Equals(b);
}
