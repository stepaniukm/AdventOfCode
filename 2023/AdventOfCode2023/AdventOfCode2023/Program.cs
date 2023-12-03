// See https://aka.ms/new-console-template for more information

using AdventOfCode2023.Day3;

var lines = File.ReadAllText("./Day3/input.txt").Split("\n");
var result = Day3.Part1(lines);
var result2 = Day3.Part2(lines);

Console.WriteLine(result);
Console.WriteLine(result2);