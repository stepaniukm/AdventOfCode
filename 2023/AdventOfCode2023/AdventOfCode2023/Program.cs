// See https://aka.ms/new-console-template for more information

using AdventOfCode2023.Day5;

var lines = File.ReadAllText("./Day5/input.txt").Split("\n");
// var timeStart = DateTime.Now;
var result = Day5.Part2(lines);
// var timeEnd = DateTime.Now;
//
// var diff = timeEnd - timeStart;
// var computationTime = diff.TotalMilliseconds * 2111618676 / 20;
// Console.WriteLine($"Time elapsed: {diff.TotalMilliseconds} ms");
// var result2 = Day5.Part2(lines);

// var result = Day5.GetOutputRanges(new Mapping("a", "b", 50, 150, 10), new MRange(45, 20));
// result.ToList().ForEach(Console.WriteLine);

Console.WriteLine(result);
// Console.WriteLine(result2);