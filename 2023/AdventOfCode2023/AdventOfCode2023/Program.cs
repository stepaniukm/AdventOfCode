// See https://aka.ms/new-console-template for more information

using AdventOfCode2023.Day9;

var lines = File.ReadAllText("./Day9/input.txt").Split("\n");
var result = Day9.Part1(lines);

// var lines2 = File.ReadAllText("./Day8/test2.txt").Split("\n");
var result2 = Day9.Part2(lines);

Console.WriteLine("-------");
Console.WriteLine(result);
Console.WriteLine(result2);