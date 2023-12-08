// See https://aka.ms/new-console-template for more information

using AdventOfCode2023.Day8;

var lines = File.ReadAllText("./Day8/input.txt").Split("\n");
var result = Day8.Part1(lines);

// var lines2 = File.ReadAllText("./Day8/test2.txt").Split("\n");
var result2 = Day8.Part2(lines);

Console.WriteLine(result);
Console.WriteLine(result2);