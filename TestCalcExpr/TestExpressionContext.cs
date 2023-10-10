using CalcExpr.Context;
using CalcExpr.Expressions;

namespace TestCalcExpr;

[TestClass]
public class TestExpressionContext
{
    [TestMethod]
    public void TestInit()
    {
        ExpressionContext context = new ExpressionContext();

        foreach (string variable in TestCases.Variables.Keys)
        {
            Assert.IsFalse(context.ContainsVariable(variable));
            Assert.AreEqual(Constant.UNDEFINED, context[variable]);
        }
    }

    [TestMethod]
    public void TestVariables()
    {
        ExpressionContext context = new ExpressionContext();

        foreach (string variable in TestCases.Variables.Keys)
        {
            context.AddVariable(variable, TestCases.Variables[variable]);
            Assert.IsTrue(context.ContainsVariable(variable));
            Assert.AreEqual(TestCases.Variables[variable], context[variable]);
            context.RemoveVariable(variable);
            Assert.IsFalse(context.ContainsVariable(variable));
            Assert.AreEqual(Constant.UNDEFINED, context[variable]);
        }
    }
}
