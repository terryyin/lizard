            // This file contains examples of C# code that are used to test the lizard tool.
            // It uses the C# 10+ features. and with some edge cases.

using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using System.Linq;
using System.Text.RegularExpressions;
using System.Numerics;

// File-scoped namespace declaration (C# 10)
namespace LizardTestExamples;

// Record struct (C# 10)
public readonly record struct ComplexityMetric(string Name, double Value, int Threshold)
{
    // Property pattern matching with 'and' pattern (C# 10)
    public bool IsAcceptable => this is { Value: <= 25.0 and > 0 };
}

// Static abstract members in interfaces (C# 11)
public interface IComplexityAnalyzer<T> where T : INumber<T>
{
    static abstract T CalculateComplexity(string code);
    static virtual string FormatResult(T complexity) => complexity.ToString() ?? "Unknown";
}

// Interface implementation and required members (C# 11)
public class CyclomaticComplexityAnalyzer : IComplexityAnalyzer<double>
{
    public static double CalculateComplexity(string code) => 
        code.Split(new[] { "if", "for", "while", "case", "&&", "||", "?", "catch" }, StringSplitOptions.None).Length - 1;
    
    // Raw string literals (C# 11)
    public static readonly string PatternTemplate = """
        (?<conditional>if|else\s+if|while|for|foreach|case|catch)\s*\((?<expr>[^)]+)\)
        |(?<operator>\|\||&&)
        |(?<ternary>\?)
        """;
}

// Class hierarchy for demonstrating complex code
public abstract class CodeAnalyzer
{
    // Generic math and constrained type parameters (C# 11)
    protected abstract Task<T> AnalyzeAsync<T>(string code) where T : INumber<T>;
    
    // Required members and init accessor (C# 9+)
    public required Dictionary<string, object> Properties { get; init; }
    
    public List<string> Warnings { get; } = new();
    
    // Parameter null checking (C# 10)
    protected void LogWarning(string message, string? source = null)
    {
        ArgumentNullException.ThrowIfNull(message);
        Warnings.Add($"{source ?? "Unknown"}: {message}");
    }
}

// Sealed class with complicated inheritance
public sealed class CSharpAnalyzer : CodeAnalyzer
{
    // Init-only setters and target-typed new (C# 9)
    private readonly Dictionary<string, Func<string, double>> _metrics = new()
    {
        ["Cyclomatic"] = code => CyclomaticComplexity(code),
        ["Nesting"] = code => NestingComplexity(code),
        ["NLOC"] = code => NonCommentLinesOfCode(code)
    };
    
    // Switch expressions with pattern matching (C# 9+)
    public string AnalyzeQuality(string code) => CalculateScoreFromMetrics(code) switch
    {
        > 90 => "Excellent",
        > 80 => "Good",
        > 70 => "Fair",
        > 60 => "Poor",
        _ => "Unacceptable"
    };
    
    // Nested pattern matching with property patterns (C# 10)
    public void ValidateMetrics(IEnumerable<ComplexityMetric> metrics)
    {
        foreach (var metric in metrics)
        {
            var action = metric switch
            {
                { Name: "Cyclomatic", Value: > 15 } => () => LogWarning($"High cyclomatic complexity: {metric.Value}", "Cyclomatic"),
                { Name: "Nesting", Value: > 5 } => () => LogWarning($"Excessive nesting: {metric.Value}", "Structure"),
                { Name: "NLOC", Value: > 100 } => () => LogWarning($"Too many lines of code: {metric.Value}", "Size"),
                { IsAcceptable: true } => () => { /* No action needed */ },
                _ => () => LogWarning($"Unknown metric issue: {metric.Name}", "General")
            };
            
            action();
        }
    }
    
    // Lambda improvements with attributes and natural type (C# 10)
    [System.Diagnostics.CodeAnalysis.MemberNotNull(nameof(_regexCache))]
    private readonly Lazy<Dictionary<string, Regex>> _regexCache = new(() => new Dictionary<string, Regex>());
    
    // Lambda with multiple statements and ref local (C# 9+)
    private double CyclomaticComplexity(string code)
    {
        var complexity = 1.0;
        var regex = GetCachedRegex(CyclomaticComplexityAnalyzer.PatternTemplate);
        var matches = regex.Matches(code);
        
        // Local function (C# 7+)
        double CalculateMatchImpact(Match match)
        {
            return match.Groups["conditional"].Success ? 1.0 :
                   match.Groups["operator"].Success ? 0.5 :
                   match.Groups["ternary"].Success ? 0.5 : 0.0;
        }
        
        foreach (Match match in matches)
        {
            complexity += CalculateMatchImpact(match);
        }
        
        return complexity;
    }
    
    // Implementation of abstract method
    protected override async Task<T> AnalyzeAsync<T>(string code)
    {
        await Task.Delay(100); // Simulate complex analysis
        
        // Covariant return (C# 9)
        if (typeof(T) == typeof(double))
        {
            var result = CyclomaticComplexity(code);
            return (T)(object)result;
        }
        
        throw new NotSupportedException($"Analysis for type {typeof(T).Name} not supported");
    }
    
    // Private helper methods
    private Regex GetCachedRegex(string pattern)
    {
        if (!_regexCache.Value.TryGetValue(pattern, out var regex))
        {
            regex = new Regex(pattern, RegexOptions.Compiled | RegexOptions.IgnorePatternWhitespace);
            _regexCache.Value[pattern] = regex;
        }
        return regex;
    }
    
    private double NestingComplexity(string code)
    {
        var lines = code.Split('\n');
        var currentDepth = 0;
        var maxDepth = 0;
        
        foreach (var line in lines)
        {
            var trimmed = line.Trim();
            currentDepth += trimmed.Count(c => c == '{');
            currentDepth -= trimmed.Count(c => c == '}');
            maxDepth = Math.Max(currentDepth, maxDepth);
        }
        
        return maxDepth;
    }
    
    private double NonCommentLinesOfCode(string code)
    {
        var commentRegex = new Regex(@"^\s*//.*$|^\s*/\*.*?\*/\s*$|^\s*$", RegexOptions.Multiline);
        var lines = code.Split('\n');
        return lines.Count(line => !commentRegex.IsMatch(line));
    }
    
    // Calculate an arbitrary score from multiple metrics
    private int CalculateScoreFromMetrics(string code)
    {
        var cyclomaticWeight = 0.5;
        var nestingWeight = 0.3;
        var nlocWeight = 0.2;
        
        var cyclomatic = Math.Min(30, CyclomaticComplexity(code));
        var nesting = Math.Min(10, NestingComplexity(code));
        var nloc = Math.Min(500, NonCommentLinesOfCode(code));
        
        // Interpolated string with conditional expression (C# 10)
        var result = 100 - (
            (cyclomatic / 30 * 100 * cyclomaticWeight) + 
            (nesting / 10 * 100 * nestingWeight) + 
            (nloc / 500 * 100 * nlocWeight)
        );
        
        return (int)Math.Round(result);
    }
}

// Sample function with dynamic implementation and using tuple returns
public static class ComplexCodeExample
{
    // Pattern matching with extended property patterns (C# 10)
    public static async Task<(bool Success, string Message, object? Result)> ProcessDataAsync<T>(
        T data, 
        Func<T, Task<object?>> processor) where T : notnull
    {
        try
        {
            object? result = data switch
            {
                string s when Regex.IsMatch(s, @"^\d+$") => await processor(data),
                string { Length: > 100 } => throw new ArgumentException("String too long for processing"),
                string s => await Task.FromResult(s.ToUpper()),
                
                Dictionary<string, object> { Count: > 0 } dict when dict.ContainsKey("priority") => 
                    await processor(data),
                
                IEnumerable<int> numbers => await Task.FromResult(numbers.Sum()),
                
                _ when data.GetType().IsValueType => await processor(data),
                
                _ => throw new NotSupportedException($"Type {data.GetType().Name} not supported")
            };
            
            return (true, "Processing completed successfully", result);
        }
        catch (Exception ex)
        {
            // Multi-line string with interpolation (C# 11)
            var errorMessage = $$"""
                Error processing data of type {{data.GetType().Name}}:
                {{ex.Message}}
                Stack trace:
                {{ex.StackTrace}}
                """;
            
            return (false, errorMessage, default);
        }
    }
    
    // Method with complex LINQ query and tuple pattern matching
    public static Dictionary<string, List<string>> AnalyzeTokenDistribution(string code, IEnumerable<string> tokenPatterns)
    {
        var tokenRegex = new Regex(string.Join("|", tokenPatterns.Select(p => $"(?<{p}>{p})")));
        var matches = tokenRegex.Matches(code);
        
        // Complex LINQ with group by and projection
        var result = matches
            .Cast<Match>()
            .SelectMany(m => m.Groups.Keys.Cast<string>()
                .Where(k => k != "0" && m.Groups[k].Success)
                .Select(k => (Type: k, Value: m.Groups[k].Value, Position: m.Groups[k].Index)))
            .GroupBy(t => t.Type)
            .ToDictionary(
                g => g.Key,
                g => g.Select(t => $"{t.Value} at position {t.Position}")
                      .OrderBy(s => s)
                      .ToList()
            );
            
        return result;
    }
}

// Extension methods for code analysis
public static class AnalysisExtensions
{
    // Extension method with complex logic
    public static async Task<IEnumerable<ComplexityMetric>> GetAllMetricsAsync(
        this CSharpAnalyzer analyzer, 
        string code)
    {
        if (string.IsNullOrWhiteSpace(code))
            return Enumerable.Empty<ComplexityMetric>();
            
        var tasks = new[]
        {
            Task.Run(() => new ComplexityMetric("Cyclomatic", CyclomaticComplexityAnalyzer.CalculateComplexity(code), 15)),
            Task.Run(() => {
                var nesting = analyzer.Properties.TryGetValue("NestingDepth", out var depth) ? Convert.ToDouble(depth) : 5.0;
                return new ComplexityMetric("Nesting", nesting, 5);
            }),
            Task.Run(() => {
                var lines = code.Split('\n').Length;
                return new ComplexityMetric("NLOC", lines, 100);
            })
        };
        
        await Task.WhenAll(tasks);
        return tasks.Select(t => t.Result);
    }
}
