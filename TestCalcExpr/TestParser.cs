using CalcExpr.Expressions;
using CalcExpr.Parsing;
using CalcExpr.Parsing.Rules;
using System.Text.RegularExpressions;

namespace TestCalcExpr;

[TestClass]
public class TestParser
{
    readonly RegexRule CUSTOM_RULE = new RegexRule("Char", @"[A-Z]", RegexOptions.None,
        (expression, token, _) => new Number(token.Value[0] - 65));
    
    /// <summary>
    /// Tests that the Parser can be initialized properly from either constructor.
    /// </summary>
    [TestMethod]
    public void TestInit()
    {
        (string Name, string? Regex)[] default_rules =
        {
            ("Operand", "({Prefix}*({Variable}|{Constant}|{Number}|{Token}){Postfix}*)"),
            ("Token", @"\[\d+\]"),
            ("FunctionCall", null),
            ("Parentheses", null),
            ("WithParentheses", @"\(|\)"),
            ("AssignBinOp", @"(?<={Operand})(?<!!)(=)(?={Operand})"),
            ("OrBinOp", @"(?<={Operand})(\|\||∨)(?={Operand})"),
            ("XorBinOp", @"(?<={Operand})(⊕)(?={Operand})"),
            ("AndBinOp", @"(?<={Operand})(&&|∧)(?={Operand})"),
            ("EqBinOp", @"(?<={Operand})(==|!=|<>|≠)(?={Operand})"),
            ("IneqBinOp", @"(?<={Operand})(>=|<=|<(?!>)|(?<!<)>|[≤≥])(?={Operand})"),
            ("AddBinOp", @"(?<={Operand})([\+\-])(?={Operand})"),
            ("MultBinOp", @"(?<={Operand})(%%|//|[*×/÷%])(?={Operand})"),
            ("ExpBinOp", @"(?<={Operand})(\^)(?={Operand})"),
            ("Prefix", @"((\+{2})|(\-{2})|[\+\-!~¬])"),
            ("Postfix", @"((\+{2})|(\-{2})|((?<![A-Za-zΑ-Ωα-ω0-9](!!)*!)!!)|[!%#])"),
            ("Constant", "(∞|(inf(inity)?)|π|pi|τ|tau|e|true|false)"),
            ("Variable", "([A-Za-zΑ-Ωα-ω]+(_[A-Za-zΑ-Ωα-ω0-9]+)*)"),
            ("Number", @"((\d+\.?\d*)|(\d*\.?\d+))"),
        };

        Parser parser = new Parser();

        for (int i = 0; i < default_rules.Length; i++)
        {
            Rule rule = parser.Grammar[i];

            Assert.AreEqual(rule.Name, default_rules[i].Name);
            Assert.AreEqual(default_rules[i].Regex, rule is RegexRule regex_rule ? regex_rule.RegularExpression : null);
        }

        parser = new Parser(new Rule[] { CUSTOM_RULE });

        Assert.IsTrue(parser.Grammar.Length == 1);
        Assert.IsTrue(parser.Grammar[0] == CUSTOM_RULE);
    }

    /// <summary>
    /// Tests that the Parser is able to properly parse strings into IExpressions.
    /// </summary>
    [TestMethod]
    public void TestParse()
    {
        foreach (TestCase test_case in TestCases.Expressions)
            Assert.AreEqual(test_case.Parsed, new Parser().Parse(test_case.ExpressionString));
    }

    /// <summary>
    /// Tests that previously parsed expressions get added to the Parser's cache, expressions can be manually added and
    /// removed from the cache, a string can be found in the cache, and the cache can be cleared.
    /// </summary>
    [TestMethod]
    public void TestCache()
    {
        Parser parser = new Parser();

        foreach (TestCase test_case in TestCases.Expressions)
        {
            parser.Parse(test_case.ExpressionString);
            Assert.IsTrue(parser.ContainsCache(test_case.ExpressionString));
            parser.RemoveCache(test_case.ExpressionString);
            Assert.IsFalse(parser.ContainsCache(test_case.ExpressionString));
        }

        (string, IExpression) pi = ("pi", new Number(3.1415926535));
        
        parser.AddCache(pi.Item1, pi.Item2);
        Assert.IsTrue(parser.ContainsCache(pi.Item1));
        parser.RemoveCache(pi.Item1);
        Assert.IsFalse(parser.ContainsCache(pi.Item1));
    }

    /// <summary>
    /// Tests that Rules can be added and removed from the Parser's grammar along with being able to change the priority
    /// of Rules.
    /// </summary>
    [TestMethod]
    public void TestGrammar()
    {
        Parser parser = new Parser();
        RegexRule tau = new RegexRule("tau", "tau", RegexOptions.None, (expression, token, _) => new Number(6.28));
        
        Assert.IsFalse(parser.Grammar.Contains(CUSTOM_RULE));
        Assert.IsFalse(parser.Grammar.Contains(tau));
        Assert.IsTrue(parser.AddGrammarRule(CUSTOM_RULE, 0));
        Assert.IsTrue(parser.Grammar[0] == CUSTOM_RULE);
        Assert.IsTrue(parser.AddGrammarRule(tau, -1));
        Assert.IsTrue(parser.Grammar.Last() == tau);
        Assert.IsTrue(parser.GrammarContains(tau.Name));
        Assert.IsTrue(parser.RemoveGrammarRule(CUSTOM_RULE.Name));
        Assert.IsFalse(parser.GrammarContains(CUSTOM_RULE.Name));
        Assert.IsTrue(parser.RemoveGrammarRuleAt(parser.Grammar.Length - 1));
        Assert.IsFalse(parser.GrammarContains(tau.Name));
    }
}
