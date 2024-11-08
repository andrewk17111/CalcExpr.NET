using CalcExpr.Attributes;
using CalcExpr.Expressions;
using CalcExpr.Extensions;
using CalcExpr.Parsing.Rules;
using CalcExpr.Parsing.Tokens;
using CalcExpr.Tokenization;
using CalcExpr.Tokenization.Tokens;
using System.Reflection;
using System.Text.RegularExpressions;
using static CalcExpr.Parsing.MatchFunctions;
using static CalcExpr.Parsing.ParseFunctions;
using static CalcExpr.Parsing.ParseMatchFunctions;

namespace CalcExpr.Parsing;

public class Parser
{
    private readonly List<IParserRule> _grammar = [];
    private readonly Dictionary<string, IExpression> _cache = [];

    public IParserRule[] Grammar => [.. _grammar];

    public string[] Cache => [.. _cache.Keys];

    /// <summary>
    /// Creates a <see cref="Parser"/> with the default grammar.
    /// </summary>
    /// <param name="build_rules">Whether or not grammar rules should be prebuilt.</param>
    public Parser(bool build_rules = true)
        : this([
            new ParserRule("Collection", ParseCollection, MatchCollection, ParseMatchCollection),
            new ParserRule("FunctionCall", ParseFunctionCall, MatchFunctionCall, ParseMatchFunctionCall),
            new ParserRule("LambdaFunction", ParseMatchLambdaFunction,
                (input, _) => {
                    if (input.Count >= 4)
                    {
                        if (input.First() is WordToken &&
                            input[1] is SymbolToken { Character: '=' } && input[2] is CloseBracketToken { BracketType: Bracket.Angle })
                        {
                            return new TokenMatch(input[..3], 0);
                        }
                        else
                        {
                            List<IToken> condensed = input.Condense(~Brackets.Angle);

                            if (condensed.Count >= 4 && condensed.First() is CondensedToken condensedParams &&
                                condensedParams.Tokens.First() is OpenBracketToken { BracketType: Bracket.Parenthesis} &&
                                condensed[1] is SymbolToken { Character: '=' } && condensed[2] is CloseBracketToken { BracketType: Bracket.Angle })
                            {
                                return new TokenMatch(input[..condensed.UncondenseIndex(3)], 0);
                            }
                            else if (condensed.Count >= 5 && condensed.First() is CondensedToken condensedParam &&
                                condensedParam.Tokens.First() is OpenBracketToken { BracketType: Bracket.Square} &&
                                condensed[1] is WordToken &&
                                condensed[2] is SymbolToken { Character: '=' } && condensed[3] is CloseBracketToken { BracketType: Bracket.Angle })
                            {
                                return new TokenMatch(input[..condensed.UncondenseIndex(4)], 0);
                            }
                        }
                    }

                    return null;
                }),
            new ParserRule("Parentheses", ParseMatchParentheses, MatchParentheses),
            new ParserRule("WithParentheses", ParseMatchWithParentheses,
                (input, _) => input.Any(x => x is OpenBracketToken { BracketType: Bracket.Parenthesis } || x is CloseBracketToken { BracketType: Bracket.Parenthesis })
                    ? new TokenMatch([], 0)
                    : null),
            new ParserRule("AssignBinOp", ParseMatchAssignmentOperator,
                (input, _) => {
                    for (int i = input.Count - 2; i > 0; i--)
                    {
                        if (input[i] is SymbolToken { Character: '=' } && input[i - 1] is not OpenBracketToken { BracketType: Bracket.Angle } &&
                            input[i - 1] is not CloseBracketToken { BracketType: Bracket.Angle } &&
                            input[i - 1].Value != "=" && input[i - 1].Value != "!" &&
                            input[i + 1] is not CloseBracketToken { BracketType: Bracket.Angle } && input[i + 1].Value != "=")
                        {
                            return new TokenMatch([input[i]], i);
                        }
                    }

                    return null;
                }),
            new ParserRule("OrBinOp", ParseMatchBinaryOperator,
                (input, _) => {
                    for (int i = input.Count - 2; i > 0; i--)
                    {
                        if (input[i] is SymbolToken { Character: '∨' })
                        {
                            return new TokenMatch([input[i]], i);
                        }
                        else if (input[i] is SymbolToken { Character: '|' } && input[i - 1] is SymbolToken { Character: '|' })
                        {
                            return new TokenMatch(input[(i - 1)..(i + 1)], i - 1);
                        }
                    }

                    return null;
                }),
            new ParserRule("XorBinOp", ParseMatchBinaryOperator,
                (input, _) => {
                    for (int i = input.Count - 2; i > 0; i--)
                    {
                        if (input[i] is SymbolToken { Character: '⊕' })
                        {
                            return new TokenMatch([input[i]], i);
                        }
                    }

                    return null;
                }),
            new ParserRule("AndBinOp", ParseMatchBinaryOperator,
                (input, _) => {
                    for (int i = input.Count - 2; i > 0; i--)
                    {
                        if (input[i] is SymbolToken { Character: '∧' })
                        {
                            return new TokenMatch([input[i]], i);
                        }
                        else if (input[i] is SymbolToken { Character: '&' } && input[i - 1] is SymbolToken { Character: '&' })
                        {
                            return new TokenMatch(input[(i - 1)..(i + 1)], i - 1);
                        }
                    }

                    return null;
                }),
            new ParserRule("EqBinOp", ParseMatchBinaryOperator,
                (input, _) => {
                    for (int i = input.Count - 2; i > 0; i--)
                    {
                        if (input[i] is SymbolToken { Character: '≠' })
                        {
                            return new TokenMatch([input[i]], i);
                        }
                        else if (input[i] is CloseBracketToken { BracketType: Bracket.Angle } &&
                            input[i - 1] is OpenBracketToken { BracketType: Bracket.Angle })
                        {
                            return new TokenMatch(input[(i - 1)..(i + 1)], i - 1);
                        }
                        else if (input[i] is SymbolToken { Character: '=' } && input[i - 1] is SymbolToken other &&
                            (other.Character == '=' || other.Character == '!'))
                        {
                            return new TokenMatch(input[(i - 1)..(i + 1)], i - 1);
                        }
                    }

                    return null;
                }),
            new ParserRule("IneqBinOp", ParseMatchBinaryOperator,
                (input, _) => {
                    for (int i = input.Count - 2; i > 0; i--)
                    {
                        if (input[i] is SymbolToken ineq && "≤≥".Contains(ineq.Character))
                        {
                            return new TokenMatch([input[i]], i);
                        }
                        else if (input[i] is CloseBracketToken { BracketType: Bracket.Angle } &&
                            input[i - 1] is OpenBracketToken { BracketType: Bracket.Angle })
                        {
                            return new TokenMatch(input[(i - 1)..(i + 1)], i - 1);
                        }
                        else if (input[i] is SymbolToken { Character: '=' } &&
                            input[i - 1] is OpenBracketToken { BracketType: Bracket.Angle } or CloseBracketToken { BracketType: Bracket.Angle })
                        {
                            return new TokenMatch(input[(i - 1)..(i + 1)], i - 1);
                        }
                        else if (input[i] is OpenBracketToken { BracketType: Bracket.Angle } &&
                            input[i + 1] is not CloseBracketToken { BracketType: Bracket.Angle })
                        {
                            return new TokenMatch([input[i]], i);
                        }
                        else if (input[i] is CloseBracketToken { BracketType: Bracket.Angle } &&
                            input[i - 1] is not OpenBracketToken { BracketType: Bracket.Angle })
                        {
                            return new TokenMatch([input[i]], i);
                        }
                    }

                    return null;
                }),
            new ParserRule("AddBinOp", ParseMatchBinaryOperator,
                (input, _) => {
                    for (int i = input.Count - 2; i > 0; i--)
                    {
                        if (input[i] is SymbolToken symbol && (symbol.Character == '+' || symbol.Character == '-') &&
                            (input[i - 1] is not SymbolToken || !((string[])["+", "-", "~", "¬", "/", "%", "*", "^"]).Contains(input[i - 1].Value)) &&
                            (input[i + 1] is not SymbolToken || !((string[])["/", "%", "*"]).Contains(input[i + 1].Value) &&
                            (i < input.Count - 2 || !((string[])["+", "-"]).Contains(input[i + 1].Value))))
                        {
                            return new TokenMatch([input[i]], i);
                        }
                    }

                    return null;
                }),
            new ParserRule("MultBinOp", ParseMatchBinaryOperator,
                (input, _) => {
                    for (int i = input.Count - 2; i > 0; i--)
                    {
                        if (input[i] is SymbolToken symbol && "*×÷".Contains(symbol.Character) &&
                            (input[i - 1] is not SymbolToken || !((string[])["+", "-", "!", "~", "¬"]).Contains(input[i - 1].Value)))
                        {
                            return new TokenMatch([input[i]], i);
                        }
                        else if (input[i] is SymbolToken { Character: '/' } && input[i - 1].Value != "/")
                        {
                            return new TokenMatch([input[i]], i);
                        }
                        else if (input[i] is SymbolToken { Character: '%' } && input[i - 1].Value != "%" && input[i + 1].Value != "%")
                        {
                            return new TokenMatch([input[i]], i);
                        }
                        else if (input[i] is SymbolToken symbol2 && "%/".Contains(symbol2.Character) && input[i - 1].Value == symbol2.Value &&
                            (input[i - 2] is not SymbolToken || !((string[])["+", "-", "!", "~", "¬", "/", "%"]).Contains(input[i - 2].Value)))
                        {
                            return new TokenMatch(input[(i - 1)..(i + 1)], i - 1);
                        }
                    }

                    return null;
                }),
            new ParserRule("ExpBinOp", ParseMatchBinaryOperator,
                (input, _) => {
                    for (int i = input.Count - 2; i > 0; i--)
                    {
                        if (input[i] is SymbolToken symbol && symbol.Character == '^' &&
                            (input[i - 1] is not SymbolToken || !((string[])["+", "-", "!", "~", "¬", "/", "%"]).Contains(input[i - 1].Value)))
                        {
                            return new TokenMatch([input[i]], i);
                        }
                    }

                    return null;
                }),
            new ParserRule("Prefix", ParseMatchPrefix,
                (input, _) => (input[0].Value == "+" && input[1].Value == "+") || (input[0].Value == "-" && input[1].Value == "-")
                    ? new TokenMatch(input[..2], 0)
                    : ((string[])["+", "-", "!", "~", "¬"]).Contains(input.First().Value)
                        ? new TokenMatch(input[..1], 0)
                        : null),
            new ParserRule("Postfix", ParseMatchPostfix,
                (input, _) => (input[^1].Value == "+" && input[^2].Value == "+") || (input[^1].Value == "-" && input[^2].Value == "-") ||
                    ((string.Join("", input.Select(x => x.Value)).Length - string.Join("", input.Select(x => x.Value)).TrimEnd('!').Length) % 2 == 0 &&
                        input[^1].Value == "!" && input[^2].Value == "!")
                    ? new TokenMatch(input[^2..], input.Count - 2)
                    : ((string[])["!", "%", "#"]).Contains(input.Last().Value)
                        ? new TokenMatch(input[^1..], input.Count - 1)
                        : null),
            new ParserRule("Indexer", ParseMatchIndexer, MatchIndexer),
            new OptionRule("Undefined", ["undefined", "dne"], ParseMatchUndefined),
            new OptionRule("Logical", ["true", "false"], ParseMatchLogical),
            new OptionRule("Infinity", ["∞", "inf", "infinity"], ParseMatchInfinity),
            new OptionRule("Constant", ["π", "pi", "τ", "tau", "empty_set", "empty", "∅", "e"], ParseMatchConstant),
            new TypeRule<WordToken>("Variable", ParseMatchVariable),
            new TypeRule<NumberToken>("Number", ParseMatchNumber),
        ],
        build_rules)
    { }

    /// <summary>
    /// Create a <see cref="Parser"/> using the specified grammar.
    /// </summary>
    /// <param name="grammar">
    /// The specified <see cref="IEnumerable{Rule}"/> to be used as the grammar of the <see cref="Parser"/>.
    /// </param>
    /// <param name="build_rules">Whether or not grammar rules should be prebuilt.</param>
    public Parser(IEnumerable<IParserRule> grammar, bool build_rules = true)
    {
        _grammar = grammar.ToList();

        if (build_rules)
            BuildGrammarRules(_grammar);
    }

    /// <summary>
    /// Parses an expression <see cref="string"/> into an <see cref="IExpression"/>.
    /// </summary>
    /// <param name="input">The expression <see cref="string"/> to parse.</param>
    /// <returns>An <see cref="IExpression"/> parsed from the specified expression <see cref="string"/>.</returns>
    /// <exception cref="ArgumentNullException"></exception>
    /// <exception cref="Exception"></exception>
    public IExpression Parse(string input)
    {
        ArgumentNullException.ThrowIfNull(input);

        if (ContainsCache(input))
            return _cache[CleanExpressionString(input)];

        // TODO: Change tokenizer initialization.
        List<IToken> tokenizedInput = new Tokenizer().Tokenize(input);
        IExpression result = Parse(tokenizedInput);

        AddCache(input, result);
        return result;
    }

    /// <summary>
    /// Parses a tokenized expression into an <see cref="IExpression"/>.
    /// </summary>
    /// <param name="input">The tokenized expression to parse.</param>
    /// <returns>An <see cref="IExpression"/> parsed from the specified expression <see cref="string"/>.</returns>
    /// <exception cref="Exception"></exception>
    public IExpression Parse(List<IToken> input)
    {
        foreach (IParserRule rule in _grammar)
        {
            if (rule.GetType().GetCustomAttribute<ReferenceRuleAttribute>() is not null)
                continue;

            IExpression? expression = rule.Parse(input, this);

            if (expression is not null)
                return expression;
        }

        throw new Exception($"The input was not in the correct format: '{input.JoinTokens()}'");
    }

    private static string CleanExpressionString(string expression)
        => Regex.Replace(expression, @"\s+", "");

    /// <summary>
    /// Determines whether the cache of the <see cref="Parser"/> contains a specified expression <see cref="string"/>.
    /// </summary>
    /// <param name="expression">The expression to locate in the cache.</param>
    /// <returns>
    /// <see langword="true"/> if the cache contains an entry with the specified expression; otherwise, 
    /// <see langword="false"/>.
    /// </returns>
    public bool ContainsCache(string expression)
        => _cache.ContainsKey(CleanExpressionString(expression));

    /// <summary>
    /// Add expression <see cref="string"/> to the cache of the <see cref="Parser"/>.
    /// </summary>
    /// <param name="key">The expression <see cref="string"/> of the cached <see cref="IExpression"/>.</param>
    /// <param name="value">The cached <see cref="IExpression"/>.</param>
    /// <returns>
    /// <see langword="true"/> if the <see cref="IExpression"/> was successfully added to the cache; otherwise, 
    /// <see langword="false"/>.
    /// </returns>
    public bool AddCache(string key, IExpression value)
    {
        try
        {
            _cache[CleanExpressionString(key)] = value;
            return true;
        }
        catch
        {
            return false;
        }
    }

    /// <summary>
    /// Removes a cached <see cref="IExpression"/> based on the specified expression <see cref="string"/>.
    /// </summary>
    /// <param name="expression">The expression <see cref="string"/> of the cached <see cref="IExpression"/>.</param>
    /// <returns>
    /// <see langword="true"/> if the <see cref="IExpression"/> was successfully removed from the cache; otherwise,
    /// <see langword="false"/>.
    /// </returns>
    public bool RemoveCache(string expression)
    {
        string clean_expression = CleanExpressionString(expression);

        return _cache.ContainsKey(clean_expression) && _cache.Remove(clean_expression);
    }

    /// <summary>
    /// Clears the cache of the parser.
    /// </summary>
    /// <returns>
    /// <see langword="true"/> if the cache was successfully cleared; otherwise <see langword="false"/>.
    /// </returns>
    public bool ClearCache()
    {
        _cache.Clear();
        return _cache.Count == 0;
    }

    /// <summary>
    /// Add <see cref="IParserRule"/> to the grammar of the <see cref="Parser"/>.
    /// </summary>
    /// <param name="rule">The <see cref="IParserRule"/> to be added to the grammar.</param>
    /// <param name="index">The index to put the <see cref="IParserRule"/> in the grammar.</param>
    /// <param name="build_rules">Whether or not grammar rules should be rebuilt.</param>
    /// <returns>
    /// <see langword="true"/> if the <see cref="IParserRule"/> was successfully added to the grammar; otherwise, 
    /// <see langword="false"/>.
    /// </returns>
    public bool AddGrammarRule(IParserRule rule, int index = -1, bool build_rules = true)
    {
        if (index < 0)
            index += _grammar.Count + 1;

        try
        {
            if (GrammarContains(rule.Name))
                return false;

            if (index <= 0)
                _grammar.Insert(0, rule);
            else if (index >= _grammar.Count)
                _grammar.Insert(_grammar.Count, rule);
            else
                _grammar.Insert(index, rule);

            if (build_rules)
                RebuildGrammarRules();

            return true;
        }
        catch
        {
            return false;
        }
    }

    /// <summary>
    /// Removes the <see cref="IParserRule"/> with the specified name from the grammar of the <see cref="Parser"/>.
    /// </summary>
    /// <param name="name">The name of the <see cref="IParserRule"/> to be removed.</param>
    /// <param name="buildRules">Whether or not grammar rules should be rebuilt.</param>
    /// <returns>
    /// The index of the removed rule if the <see cref="IParserRule"/> was successfully removed; otherwise, -1.
    /// </returns>
    public int RemoveGrammarRule(string name, bool buildRules = true)
    {
        for (int i = 0; i < _grammar.Count; i++)
        {
            if (_grammar[i].Name == name)
            {
                if (RemoveGrammarRuleAt(i, buildRules))
                    return i;
                else
                    break;
            }
        }

        return -1;
    }

    /// <summary>
    /// Removes the <see cref="IParserRule"/> at the specified index from the grammar of the <see cref="Parser"/>.
    /// </summary>
    /// <param name="index">The index for the <see cref="IParserRule"/> to be removed.</param>
    /// <param name="build_rules">Whether or not grammar rules should be rebuilt.</param>
    /// <returns>
    /// <see langword="true"/> if the <see cref="IParserRule"/> was successfully removed; otherwise, <see langword="false"/>.
    /// </returns>
    public bool RemoveGrammarRuleAt(int index, bool build_rules = true)
    {
        try
        {
            _grammar.RemoveAt(index);

            if (build_rules)
                RebuildGrammarRules();

            return true;
        }
        catch
        {
            return false;
        }
    }

    /// <summary>
    /// Replaces the current <see cref="IParserRule"/> with the specified name with a new <see cref="IParserRule"/>.
    /// </summary>
    /// <param name="newRule">The new rule to replace the old rule.</param>
    /// <param name="buildRules">Whether or not grammar rules should be rebuilt.</param>
    /// <returns>
    /// The index of the replaced rule if the <see cref="IParserRule"/> was successfully replaced; otherwise, -1.
    /// </returns>
    public int ReplaceGrammarRule(IParserRule newRule, bool buildRules = true)
    {
        for (int i = 0; i < _grammar.Count; i++)
        {
            if (_grammar[i].Name == newRule.Name)
            {
                _grammar[i] = newRule;
                return i;
            }
        }

        if (buildRules)
            RebuildGrammarRules();

        return -1;
    }

    /// <summary>
    /// Determines whether a rule with the specified name is in the grammar of the <see cref="Parser"/>.
    /// </summary>
    /// <param name="name">The name of the <see cref="IParserRule"/> to find.</param>
    /// <returns>
    /// <see langword="true"/> if the <see cref="IParserRule"/> was successfully found; otherwise, <see langword="false"/>.
    /// </returns>
    public bool GrammarContains(string name)
    {
        foreach (IParserRule rule in _grammar)
            if (rule.Name == name)
                return true;

        return false;
    }

    /// <summary>
    /// Gets a <see cref="IParserRule"/> from the grammar based on the specified name.
    /// </summary>
    /// <param name="name">The name of the grammar rule.</param>
    /// <returns>The <see cref="IParserRule"/> in the grammar with the name <paramref name="name"/>.</returns>
    public IParserRule? GetGrammarRule(string name)
    {
        int idx = 0;

        while (idx < _grammar.Count && _grammar[idx].Name != name)
            idx++;

        return GetGrammarRule(idx);
    }

    /// <summary>
    /// Gets a <see cref="IParserRule"/> from the grammar based on the specified index.
    /// </summary>
    /// <param name="index">The index of the grammar rule.</param>
    /// <returns>The <see cref="IParserRule"/> in the grammar at the index of <paramref name="index"/>.</returns>
    public IParserRule? GetGrammarRule(int index)
        => index >= 0 && index < _grammar.Count ? _grammar[index] : null;

    /// <summary>
    /// Rebuilds all of the grammar rules in the <see cref="Parser"/>.
    /// </summary>
    public void RebuildGrammarRules()
        => BuildGrammarRules(_grammar);

    private static void BuildGrammarRules(IEnumerable<IParserRule> rules)
    {
        //foreach (IParserRule rule in rules)
        //    if (rule is NestedRegexRule regex_rule)
        //        regex_rule.Build(rules);
    }
}
