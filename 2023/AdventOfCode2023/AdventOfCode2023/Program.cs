// See https://aka.ms/new-console-template for more information

using AdventOfCode2023.Day2;

var lines = File.ReadAllText("./Day2/input.txt").Split("\n").ToList();
var result = Day2.Part1(lines);
var result2 = Day2.Part2(lines);

Console.WriteLine(result);
Console.WriteLine(result2);
