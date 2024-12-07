using CalcExpr.Expressions;

namespace CalcExpr.FunctionAttributes.PreprocessAttributes;

public abstract class PreprocessAttribute : FunctionAttribute
{
    public abstract IExpression Preprocess(IExpression expression);
}
