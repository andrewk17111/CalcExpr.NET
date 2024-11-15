﻿using CalcExpr.Tokenization;
using CalcExpr.Tokenization.Tokens;
using System.Collections.Immutable;
using TestCalcExpr.TestData;
using TestCalcExpr.TestUtils;

namespace TestCalcExpr.Tokenization;

[TestClass]
public class TestTokenizer
{
    [TestMethod]
    public void TestEmptyTokenizer()
    {
        string input = "Hello World!";
        Tokenizer tokenizer = new Tokenizer([], false);
        ImmutableArray<IToken> tokens = tokenizer.Tokenize(input);

        Assert.AreEqual(input, string.Join("", tokens.Select(t => t.Value)));
    }

    /// <summary>
    /// Tests that the Tokenizer is able to properly tokenize strings.
    /// </summary>
    [TestMethod]
    public void TestTokenize()
    {
        TokenizeTestCases(TestCases.Expressions);
    }

    /// <summary>
    /// Tests that the Tokenizer is able to properly tokenize strings for collections.
    /// </summary>
    [TestMethod]
    public void TestTokenizeCollections()
    {
        TokenizeTestCases(TestCases.Collections);
    }

    private static void TokenizeTestCases(IEnumerable<TestCase> testCases)
    {
        Tokenizer tokenizer = new Tokenizer();

        foreach (TestCase testCase in testCases)
        {
            ImmutableArray<IToken> tokenized = tokenizer.Tokenize(testCase.ExpressionString);

            Assert.AreEqual(testCase.Tokenized.Length, tokenized.Length, $"The lengths of the tokenized results do not match for test case '{testCase.ExpressionString}'");

            for (int i = 0; i < tokenized.Length; i++)
                Assert.AreEqual(testCase.Tokenized[i], tokenized[i], $"Token did not match for test case '{testCase.ExpressionString}'");
        }
    }
}
