using CalcExpr.Expressions;
using CalcExpr.Parsing;
using System.Text.RegularExpressions;

namespace TestCalcExpr;

[TestClass]
public class TestParser
{
    readonly Rule CUSTOM_RULE = new Rule(@"[A-Z", (expression, token) => new Number(token.Value[0] - 65));
    readonly Dictionary<string, IExpression> EXPRESSIONS = new Dictionary<string, IExpression>()
    {
        { ".1", new Number(0.1) },
        { "0.1", new Number(0.1) },
        { "1.", new Number(1) },
        { "1", new Number(1) },
        { "+1", new UnaryOperator("+", true, new Number(1)) },
        { "-1", new UnaryOperator("-", true, new Number(1)) },
        { "!1", new UnaryOperator("!", true, new Number(1)) },
        { "~2", new UnaryOperator("~", true, new Number(2)) },
        { "¬0", new UnaryOperator("¬", true, new Number(0)) },
        { "5!", new UnaryOperator("!", false, new Number(5)) },
        { "1%", new UnaryOperator("%", false, new Number(1)) },
        { "~!1", new UnaryOperator("~", true, new UnaryOperator("!", true, new Number(1))) },
        { "2!%", new UnaryOperator("%", false, new UnaryOperator("!", false, new Number(2))) },
        { "-5%", new UnaryOperator("-", true, new UnaryOperator("%", false, new Number(5))) },
        { "1+2.0", new BinaryOperator("+", new Number(1), new Number(2)) },
        { "0 + 0 * 2", new BinaryOperator("+", new Number(0), new BinaryOperator("*", new Number(0), new Number(2))) },
        { "1.0-2", new BinaryOperator("-", new Number(1), new Number(2)) },
        { "2*3", new BinaryOperator("*", new Number(2), new Number(3)) },
        { "6×7", new BinaryOperator("×", new Number(6), new Number(7)) },
        { "1/2", new BinaryOperator("/", new Number(1), new Number(2)) },
        { "2÷2", new BinaryOperator("÷", new Number(2), new Number(2)) },
        { "2^3", new BinaryOperator("^", new Number(2), new Number(3)) },
        { "13%12", new BinaryOperator("%", new Number(13), new Number(12)) },
        { "13.2%%12.5", new BinaryOperator("%%", new Number(13.2), new Number(12.5)) },
        { "13//12", new BinaryOperator("//", new Number(13), new Number(12)) },
        { "1&&2", new BinaryOperator("&&", new Number(1), new Number(2)) },
        { "0∧0", new BinaryOperator("∧", new Number(0), new Number(0)) },
        { "1||2", new BinaryOperator("||", new Number(1), new Number(2)) },
        { "0∨0", new BinaryOperator("∨", new Number(0), new Number(0)) },
        { "1⊕1", new BinaryOperator("⊕", new Number(1), new Number(1)) },
        { "1==2", new BinaryOperator("==", new Number(1), new Number(2)) },
        { "1!=2", new BinaryOperator("!=", new Number(1), new Number(2)) },
        { "1≠1", new BinaryOperator("≠", new Number(1), new Number(1)) },
        { "1<>2", new BinaryOperator("<>", new Number(1), new Number(2)) },
        { "1<2", new BinaryOperator("<", new Number(1), new Number(2)) },
        { "1>2", new BinaryOperator(">", new Number(1), new Number(2)) },
        { "1>=2", new BinaryOperator(">=", new Number(1), new Number(2)) },
        { "2≥2", new BinaryOperator("≥", new Number(2), new Number(2)) },
        { "1<=2", new BinaryOperator("<=", new Number(1), new Number(2)) },
        { "1≤1", new BinaryOperator("≤", new Number(1), new Number(1)) },
        { "1+2-3*4/5^7&&8||9⊕10==11", new BinaryOperator("||", new BinaryOperator("&&", new BinaryOperator("-",
            new BinaryOperator("+", new Number(1), new Number(2)), new BinaryOperator("/", new BinaryOperator("*",
                new Number(3), new Number(4)), new BinaryOperator("^", new Number(5), new Number(7)))), new Number(8)),
            new BinaryOperator("⊕", new Number(9), new BinaryOperator("==", new Number(10), new Number(11)))) },
        { "+1+-2.0", new BinaryOperator("+", new UnaryOperator("+", true, new Number(1)), new UnaryOperator("-", true,
            new Number(2))) },
        { "1.0!-2%", new BinaryOperator("-", new UnaryOperator("!", false, new Number(1)), new UnaryOperator("%", false,
            new Number(2))) },
        { "!¬2*-+3", new BinaryOperator("*", new UnaryOperator("!", true, new UnaryOperator("¬", true, new Number(2))),
            new UnaryOperator("-", true, new UnaryOperator("+", true, new Number(3)))) },
        { "1%%/2!!", new BinaryOperator("/", new UnaryOperator("%", false, new UnaryOperator("%", false,
            new Number(1))), new UnaryOperator("!", false, new UnaryOperator("!", false, new Number(2)))) },
        { "-13!%!12%", new BinaryOperator("%", new UnaryOperator("-", true, new UnaryOperator("!", false,
            new Number(13))), new UnaryOperator("!", true, new UnaryOperator("%", false, new Number(12)))) },
        { "1+((2-3)*(4/5))^7&&8||9⊕10==11", new BinaryOperator("||", new BinaryOperator("&&", new BinaryOperator("+",
            new Number(1), new BinaryOperator("^", new Parentheses(new BinaryOperator("*", new Parentheses(
                new BinaryOperator("-", new Number(2), new Number(3))), new Parentheses( new BinaryOperator("/",
                    new Number(4), new Number(5))))), new Number(7))), new Number(8)), new BinaryOperator("⊕",
                        new Number(9), new BinaryOperator("==", new Number(10), new Number(11)))) },
    };

    /// <summary>
    /// Tests that the Parser can be initialized properly from either constructor.
    /// </summary>
    [TestMethod]
    public void TestInit()
    {
        const string OPERAND = @"([\+\-!~¬]*((\d+\.?\d*)|(\d*\.?\d+)|\[\d+\])[%!]*)";

        string[] default_rules =
        {
            @$"(?<={OPERAND})(\|\||∨)(?={OPERAND})",
            @$"(?<={OPERAND})(⊕)(?={OPERAND})",
            @$"(?<={OPERAND})(&&|∧)(?={OPERAND})",
            @$"(?<={OPERAND})(==|!=|<>|≠)(?={OPERAND})",
            @$"(?<={OPERAND})(>=|<=|<(?!>)|(?<!<)>|[≤≥])(?={OPERAND})",
            @$"(?<={OPERAND})([\+\-])(?={OPERAND})",
            @$"(?<={OPERAND})(%%|//|[*×/÷%])(?={OPERAND})",
            @$"(?<={OPERAND})(\^)(?={OPERAND})",
            @"(?<=^\s*)[\+\-!~¬]",
            @"[%!](?=\s*$)",
            @"(?<=^\s*)((\d+\.?\d*)|(\d*\.?\d+))(?=\s*$)",
        };

        Parser parser = new Parser();

        for (int i = 0; i < default_rules.Length; i++)
            Assert.IsTrue(parser.Grammar[i].RegularExpression == default_rules[i]);

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
        foreach (string expression in EXPRESSIONS.Keys)
            Assert.AreEqual(EXPRESSIONS[expression], new Parser().Parse(expression));
    }

    /// <summary>
    /// Tests that previously parsed expressions get added to the Parser's cache, expressions can be manually added and
    /// removed from the cache, a string can be found in the cache, and the cache can be cleared.
    /// </summary>
    [TestMethod]
    public void TestCache()
    {
        Parser parser = new Parser();

        foreach (string expression in EXPRESSIONS.Keys)
        {
            parser.Parse(expression);

            bool cached = parser.ContainsCache(expression);

            if (Regex.IsMatch(expression, @"\(|\)"))
            {
                Assert.IsFalse(cached);
            }
            else
            {
                Assert.IsTrue(cached);
                parser.RemoveCache(expression);
                Assert.IsFalse(parser.ContainsCache(expression));
            }
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
        Rule tau = new Rule("tau", (expression, token) => new Number(6.28));
        
        Assert.IsFalse(parser.Grammar.Contains(CUSTOM_RULE));
        Assert.IsFalse(parser.Grammar.Contains(tau));
        Assert.IsTrue(parser.AddGrammarRule(CUSTOM_RULE, 0));
        Assert.IsTrue(parser.Grammar[0] == CUSTOM_RULE);
        Assert.IsTrue(parser.AddGrammarRule(tau.RegularExpression, tau.Parse, -1));
        Assert.IsTrue(parser.Grammar.Last() == tau);
        Assert.IsTrue(parser.GrammarContains(tau.RegularExpression));
        Assert.IsTrue(parser.RemoveGrammarRule(CUSTOM_RULE.RegularExpression));
        Assert.IsFalse(parser.GrammarContains(CUSTOM_RULE.RegularExpression));
        Assert.IsTrue(parser.RemoveGrammarRuleAt(parser.Grammar.Length - 1));
        Assert.IsFalse(parser.GrammarContains(tau.RegularExpression));
    }
}
