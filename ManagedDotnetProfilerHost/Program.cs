using System.Diagnostics;
using Microsoft.Diagnostics.NETCore.Client;
using Microsoft.Extensions.Logging;

Guid ProfilerGuid = Guid.Parse("846F5F1C-F9AE-4B07-969E-05C26BC060D8");
const string ProfilerName = "ManagedDotnetProfiler";
const string LikelyRelativePath = @"..\ManagedDotnetProfiler\bin\Release\net7.0\win-x64\publish\";

void AttachProfiler(int processId)
{
    var libraryName = ProfilerName + ".dll";
    var relativePath = Path.Combine(Directory.GetCurrentDirectory(), LikelyRelativePath, libraryName);
    var libraryPath = Path.GetFullPath(relativePath);

    if (!File.Exists(libraryPath))
    {
        Console.WriteLine($"[Host] libraryPath doesn't exist '{libraryPath}'");
        return;
    }

    var client = new DiagnosticsClient(processId);
    client.AttachProfiler(TimeSpan.FromSeconds(10), ProfilerGuid, libraryPath);
}

void AttachToTestApp()
{
    var procs = Process.GetProcessesByName("TestWebApp");
    var proc = procs.FirstOrDefault();

    Console.WriteLine($"[Host] got {procs.Length} using {proc?.Id}");

    if (proc != null)
    {
        AttachProfiler(proc.Id);
    }
}

AttachToTestApp();