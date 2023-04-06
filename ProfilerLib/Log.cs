using System;

namespace ProfilerLib;

public static class Log
{
    public static void WriteLine(string message)
    {
        Console.WriteLine($"[Profiler] {message}");
    }
}