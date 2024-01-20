using CalcExpr.Expressions;
using TestCalcExpr.TestUtils;

namespace TestCalcExpr.TestData;

public static partial class TestCases
{
    public readonly static FunctionTestCase[] LogicalFunctions =
    [
        new FunctionTestCase("and", new Dictionary<IExpression[], IExpression>
        {
            { [TestValues.UNDEFINED, Constant.TRUE], TestValues.UNDEFINED },
            { [TestValues.INFINITY, Constant.TRUE], TestValues.ONE },
            { [TestValues.NEGATIVE_INFINITY, Constant.TRUE], TestValues.ONE },
            { [TestValues.ZERO, Constant.TRUE], TestValues.ZERO },
            { [TestValues.ONE, Constant.TRUE], TestValues.ONE },
            { [TestValues.NEGATIVE_ONE, Constant.TRUE], TestValues.ONE },
            { [TestValues.INTEGER, Constant.TRUE], TestValues.ONE },
            { [TestValues.NEGATIVE_INTEGER, Constant.TRUE], TestValues.ONE },
            { [TestValues.DECIMAL, Constant.TRUE], TestValues.ONE },
            { [TestValues.NEGATIVE_DECIMAL, Constant.TRUE], TestValues.ONE },
            { [TestValues.UNDEFINED, Constant.FALSE], TestValues.UNDEFINED },
            { [TestValues.INFINITY, Constant.FALSE], TestValues.ZERO },
            { [TestValues.NEGATIVE_INFINITY, Constant.FALSE], TestValues.ZERO },
            { [TestValues.ZERO, Constant.FALSE], TestValues.ZERO },
            { [TestValues.ONE, Constant.FALSE], TestValues.ZERO },
            { [TestValues.NEGATIVE_ONE, Constant.FALSE], TestValues.ZERO },
            { [TestValues.INTEGER, Constant.FALSE], TestValues.ZERO },
            { [TestValues.NEGATIVE_INTEGER, Constant.FALSE], TestValues.ZERO },
            { [TestValues.DECIMAL, Constant.FALSE], TestValues.ZERO },
            { [TestValues.NEGATIVE_DECIMAL, Constant.FALSE], TestValues.ZERO },
        }),
        new FunctionTestCase("or", new Dictionary<IExpression[], IExpression>
        {
            { [TestValues.UNDEFINED, Constant.TRUE], TestValues.UNDEFINED },
            { [TestValues.INFINITY, Constant.TRUE], TestValues.ONE },
            { [TestValues.NEGATIVE_INFINITY, Constant.TRUE], TestValues.ONE },
            { [TestValues.ZERO, Constant.TRUE], TestValues.ONE },
            { [TestValues.ONE, Constant.TRUE], TestValues.ONE },
            { [TestValues.NEGATIVE_ONE, Constant.TRUE], TestValues.ONE },
            { [TestValues.INTEGER, Constant.TRUE], TestValues.ONE },
            { [TestValues.NEGATIVE_INTEGER, Constant.TRUE], TestValues.ONE },
            { [TestValues.DECIMAL, Constant.TRUE], TestValues.ONE },
            { [TestValues.NEGATIVE_DECIMAL, Constant.TRUE], TestValues.ONE },
            { [TestValues.UNDEFINED, Constant.FALSE], TestValues.UNDEFINED },
            { [TestValues.INFINITY, Constant.FALSE], TestValues.ONE },
            { [TestValues.NEGATIVE_INFINITY, Constant.FALSE], TestValues.ONE },
            { [TestValues.ZERO, Constant.FALSE], TestValues.ZERO },
            { [TestValues.ONE, Constant.FALSE], TestValues.ONE },
            { [TestValues.NEGATIVE_ONE, Constant.FALSE], TestValues.ONE },
            { [TestValues.INTEGER, Constant.FALSE], TestValues.ONE },
            { [TestValues.NEGATIVE_INTEGER, Constant.FALSE], TestValues.ONE },
            { [TestValues.DECIMAL, Constant.FALSE], TestValues.ONE },
            { [TestValues.NEGATIVE_DECIMAL, Constant.FALSE], TestValues.ONE },
        }),
        new FunctionTestCase("xor", new Dictionary<IExpression[], IExpression>
        {
            { [TestValues.UNDEFINED, Constant.TRUE], TestValues.UNDEFINED },
            { [TestValues.INFINITY, Constant.TRUE], TestValues.ZERO },
            { [TestValues.NEGATIVE_INFINITY, Constant.TRUE], TestValues.ZERO },
            { [TestValues.ZERO, Constant.TRUE], TestValues.ONE },
            { [TestValues.ONE, Constant.TRUE], TestValues.ZERO },
            { [TestValues.NEGATIVE_ONE, Constant.TRUE], TestValues.ZERO },
            { [TestValues.INTEGER, Constant.TRUE], TestValues.ZERO },
            { [TestValues.NEGATIVE_INTEGER, Constant.TRUE], TestValues.ZERO },
            { [TestValues.DECIMAL, Constant.TRUE], TestValues.ZERO },
            { [TestValues.NEGATIVE_DECIMAL, Constant.TRUE], TestValues.ZERO },
            { [TestValues.UNDEFINED, Constant.FALSE], TestValues.UNDEFINED },
            { [TestValues.INFINITY, Constant.FALSE], TestValues.ONE },
            { [TestValues.NEGATIVE_INFINITY, Constant.FALSE], TestValues.ONE },
            { [TestValues.ZERO, Constant.FALSE], TestValues.ZERO },
            { [TestValues.ONE, Constant.FALSE], TestValues.ONE },
            { [TestValues.NEGATIVE_ONE, Constant.FALSE], TestValues.ONE },
            { [TestValues.INTEGER, Constant.FALSE], TestValues.ONE },
            { [TestValues.NEGATIVE_INTEGER, Constant.FALSE], TestValues.ONE },
            { [TestValues.DECIMAL, Constant.FALSE], TestValues.ONE },
            { [TestValues.NEGATIVE_DECIMAL, Constant.FALSE], TestValues.ONE },
        }),
        new FunctionTestCase("not", new Dictionary<IExpression, IExpression>
        {
            { TestValues.UNDEFINED, TestValues.UNDEFINED },
            { TestValues.INFINITY, TestValues.ZERO },
            { TestValues.NEGATIVE_INFINITY, TestValues.ZERO },
            { TestValues.ZERO, TestValues.ONE },
            { TestValues.ONE, TestValues.ZERO },
            { TestValues.NEGATIVE_ONE, TestValues.ZERO },
            { TestValues.INTEGER, TestValues.ZERO },
            { TestValues.NEGATIVE_INTEGER, TestValues.ZERO },
            { TestValues.SMALL_DECIMAL, TestValues.ZERO },
            { TestValues.NEGATIVE_SMALL_DECIMAL, TestValues.ZERO },
            { TestValues.DECIMAL, TestValues.ZERO },
            { TestValues.NEGATIVE_DECIMAL, TestValues.ZERO },
        }),
        new FunctionTestCase("bool", new Dictionary<IExpression, IExpression>
        {
            { TestValues.UNDEFINED, TestValues.UNDEFINED },
            { TestValues.INFINITY, TestValues.ONE },
            { TestValues.NEGATIVE_INFINITY, TestValues.ONE },
            { TestValues.ZERO, TestValues.ZERO },
            { TestValues.ONE, TestValues.ONE },
            { TestValues.NEGATIVE_ONE, TestValues.ONE },
            { TestValues.INTEGER, TestValues.ONE },
            { TestValues.NEGATIVE_INTEGER, TestValues.ONE },
            { TestValues.SMALL_DECIMAL, TestValues.ONE },
            { TestValues.NEGATIVE_SMALL_DECIMAL, TestValues.ONE },
            { TestValues.DECIMAL, TestValues.ONE },
            { TestValues.NEGATIVE_DECIMAL, TestValues.ONE },
        }),
        new FunctionTestCase("if", new Dictionary<IExpression[], IExpression>
        {
            { [TestValues.UNDEFINED, TestValues.DECIMAL, TestValues.INTEGER], TestValues.UNDEFINED },
            { [TestValues.ONE, TestValues.DECIMAL, TestValues.INTEGER], TestValues.DECIMAL },
            { [TestValues.ZERO, TestValues.DECIMAL, TestValues.INTEGER], TestValues.INTEGER },
        }),
        new FunctionTestCase("is_na", new Dictionary<IExpression, IExpression>
        {
            { TestValues.UNDEFINED, TestValues.ONE },
            { TestValues.INFINITY, TestValues.ZERO },
            { TestValues.NEGATIVE_INFINITY, TestValues.ZERO },
            { TestValues.ZERO, TestValues.ZERO },
            { TestValues.ONE, TestValues.ZERO },
            { TestValues.NEGATIVE_ONE, TestValues.ZERO },
            { TestValues.INTEGER, TestValues.ZERO },
            { TestValues.NEGATIVE_INTEGER, TestValues.ZERO },
            { TestValues.DECIMAL, TestValues.ZERO },
            { TestValues.NEGATIVE_DECIMAL, TestValues.ZERO },
        }),
        new FunctionTestCase("is_num", new Dictionary<IExpression, IExpression>
        {
            { TestValues.UNDEFINED, TestValues.ZERO },
            { TestValues.INFINITY, TestValues.ZERO },
            { TestValues.NEGATIVE_INFINITY, TestValues.ZERO },
            { TestValues.ZERO, TestValues.ONE },
            { TestValues.ONE, TestValues.ONE },
        }),
    ];
}
