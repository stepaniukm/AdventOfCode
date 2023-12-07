// See https://aka.ms/new-console-template for more information

using AdventOfCode2023.Day7;

var lines = File.ReadAllText("./Day7/input.txt").Split("\n");

var result = Day7.Part1(lines);
var result2 = Day7.Part2(lines);

Console.WriteLine(result);
Console.WriteLine(result2);