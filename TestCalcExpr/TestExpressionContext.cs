using CalcExpr.Context;
using CalcExpr.Expressions;

namespace TestCalcExpr;

[TestClass]
public class TestExpressionContext
{
    [TestMethod]
    public void TestInit()
    {
        ExpressionContext context = new ExpressionContext(TestCases.Variables, TestCases.Functions);

        foreach (string variable in TestCases.Variables.Keys)
        {
            Assert.IsTrue(context.ContainsVariable(variable));

            if (TestCases.Variables[variable] is Function)
                Assert.IsTrue(context.ContainsFunction(variable));
        }

        foreach (string function in TestCases.Functions.Keys)
        {
            Assert.IsTrue(context.ContainsVariable(function));
            Assert.IsTrue(context.ContainsFunction(function));
        }
    }

    [TestMethod]
    public void TestVariables()
    {
        ExpressionContext context = new ExpressionContext();

        foreach (string variable in TestCases.Variables.Keys)
        {
            context.SetVariable(variable, TestCases.Variables[variable]);
            Assert.IsTrue(context.ContainsVariable(variable));

            if (TestCases.Variables[variable].GetType() == typeof(Function))
                Assert.IsTrue(context.ContainsFunction(variable));

            Assert.AreEqual(TestCases.Variables[variable], context[variable]);
            context.RemoveVariable(variable);
            Assert.IsFalse(context.ContainsVariable(variable));
            Assert.AreEqual(Constant.UNDEFINED, context[variable]);
        }
    }

    [TestMethod]
    public void TestFunctions()
    {
        ExpressionContext context = new ExpressionContext();

        foreach (string function in TestCases.Functions.Keys)
        {
            context.SetVariable(function, TestCases.Variables[function]);
            Assert.IsTrue(context.ContainsVariable(function));
            Assert.IsTrue(context.ContainsFunction(function));
            Assert.AreEqual(TestCases.Functions[function], context[function]);
            context.RemoveVariable(function);
            Assert.IsFalse(context.ContainsVariable(function));
            Assert.IsFalse(context.ContainsFunction(function));
            Assert.AreEqual(Constant.UNDEFINED, context[function]);

            context.SetFunction(function, TestCases.Functions[function]);
            Assert.IsTrue(context.ContainsVariable(function));
            Assert.IsTrue(context.ContainsFunction(function));
            Assert.AreEqual(TestCases.Functions[function], context[function]);
            context.RemoveFunction(function);
            Assert.IsFalse(context.ContainsVariable(function));
            Assert.IsFalse(context.ContainsFunction(function));
            Assert.AreEqual(Constant.UNDEFINED, context[function]);
        }
    }
}
