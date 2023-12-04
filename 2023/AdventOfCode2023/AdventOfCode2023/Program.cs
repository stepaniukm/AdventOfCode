// See https://aka.ms/new-console-template for more information

using AdventOfCode2023.Day4;

var lines = File.ReadAllText("./Day4/input.txt").Split("\n");
var result = Day4.Part1(lines);
var result2 = Day4.Part2(lines);

Console.WriteLine(result);
Console.WriteLine(result2);