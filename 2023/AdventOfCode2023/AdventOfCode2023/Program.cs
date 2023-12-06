// See https://aka.ms/new-console-template for more information

using AdventOfCode2023.Day6;

var lines = File.ReadAllText("./Day6/input.txt").Split("\n");

var result = Day6.Part1(lines);
var result2 = Day6.Part2(lines);

Console.WriteLine(result);
Console.WriteLine(result2);