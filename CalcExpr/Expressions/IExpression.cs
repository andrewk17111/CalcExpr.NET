using CalcExpr.Context;

namespace CalcExpr.Expressions;

public interface IExpression
{
    /// <summary>
    /// Evaluates and calculates the value of the <see cref="IExpression"/>.
    /// </summary>
    /// <returns>
    /// A new <see cref="IExpression"/> containing the value of the current <see cref="IExpression"/>.
    /// </returns>
    public IExpression Evaluate();

    /// <summary>
    /// Evaluates and calculates the value of the <see cref="IExpression"/>.
    /// </summary>
    /// <param name="context">The values for each variable that could appear in the <see cref="IExpression"/>.</param>
    /// <returns>
    /// A new <see cref="IExpression"/> containing the value of the current <see cref="IExpression"/>.
    /// </returns>
    public IExpression Evaluate(ExpressionContext context);

    /// <summary>
    /// Evaluates and calculates one step towards the value of the <see cref="IExpression"/>.
    /// </summary>
    /// <returns>
    /// A new <see cref="IExpression"/> containing the partially evaluated value of the current 
    /// <see cref="IExpression"/>.
    /// </returns>
    public IExpression StepEvaluate();

    /// <summary>
    /// Evaluates and calculates one step towards the value of the <see cref="IExpression"/>.
    /// </summary>
    /// <param name="context">The values for each variable that could appear in the <see cref="IExpression"/>.</param>
    /// <returns>
    /// A new <see cref="IExpression"/> containing the partially evaluated value of the current 
    /// <see cref="IExpression"/>.
    /// </returns>
    public IExpression StepEvaluate(ExpressionContext context);

    /// <summary>
    /// Creates a new <see cref="IExpression"/> with the same values of the current <see cref="IExpression"/>.
    /// </summary>
    /// <returns>A new copy of the current <see cref="IExpression"/>.</returns>
    public IExpression Clone();

    /// <summary>
    /// Returns a <see cref="string"/> that represents the current <see cref="IExpression"/>.
    /// </summary>
    /// <returns>A <see cref="string"/> that represents the current <see cref="IExpression"/>.</returns>
    public string ToString();

    /// <summary>
    /// Returns a <see cref="string"/> that represents the current <see cref="IExpression"/> using the specified 
    /// format <see cref="string"/> for number formats.
    /// </summary>
    /// <param name="format"></param>
    /// <returns>
    /// A <see cref="string"/> that represents the current <see cref="IExpression"/> using the specified format 
    /// <see cref="string"/>.
    /// </returns>
    public string ToString(string? format);
}
