using CalcExpr.Exceptions;
using CalcExpr.Expressions;
using System.Text.RegularExpressions;

namespace CalcExpr.Parsing.Rules;

public class NestedRegexRule : RegexRule
{
    public NestedRegexRule(string name, string regex, RegexOptions options,
        Func<string, Token, Parser, IExpression> parse)
        : base(name, regex, options, parse)
    { }

    public override Token? Match(string input, IEnumerable<Rule> rules)
    {
        Dictionary<string, string> preprocessed_rules = new Dictionary<string, string>();
        string regex = ReplaceRules(RegularExpression, rules, ref preprocessed_rules);

        return FindMatch(input, regex, Options);
    }

    private static string ReplaceRules(string regular_expression, IEnumerable<Rule> rules,
        ref Dictionary<string, string> preprocessed_rules)
    {
        List<Match> matches = Regex.Matches(regular_expression, @"(?<=[^\\](\\\\)*{)\w+(?=})").Distinct().ToList();

        if (matches.Any())
        {
            string regex = regular_expression;
            List<string> rule_names = rules.Select(x => x.Name).ToList();

            foreach (Match match in matches.ToArray())
            {
                int index = rule_names.IndexOf(match.Value);
                
                if (preprocessed_rules.ContainsKey(match.Value))
                {
                    regex = Regex.Replace(regex, @$"(?<=[^\\](\\\\)*{{){match.Value}(?=}})",
                        preprocessed_rules[match.Value]);
                }
                else if (index > -1)
                {
                    if (rules.ElementAt(index) is NestedRegexRule nrr)
                    {
                        string sub_regex = ReplaceRules(nrr.RegularExpression, rules, ref preprocessed_rules);

                        regex = Regex.Replace(regex, @$"(?<=[^\\](\\\\)*){{{match.Value}}}", sub_regex);
                        preprocessed_rules.Add(match.Value, sub_regex);
                    }
                    else if (rules.ElementAt(index) is RegexRule rr)
                    {
                        regex = Regex.Replace(regex, @$"(?<=[^\\](\\\\)*){{{match.Value}}}",
                            rr.RegularExpression);
                    }
                    else
                    {
                        throw new NonRegexRuleException(match.Value, rules.ElementAt(index).GetType());
                    }

                    rule_names.Remove(match.Name);
                    matches.Remove(match);
                }
            }

            return regex;
        }

        return regular_expression;
    }

    public override bool Equals(object? obj)
        => obj is not null && obj is RegexRule a && RegularExpression == a.RegularExpression;

    public override int GetHashCode()
        => RegularExpression.GetHashCode();

    public static bool operator ==(NestedRegexRule a, NestedRegexRule b)
        => a.Equals(b);

    public static bool operator !=(NestedRegexRule a, NestedRegexRule b)
        => !a.Equals(b);
}
