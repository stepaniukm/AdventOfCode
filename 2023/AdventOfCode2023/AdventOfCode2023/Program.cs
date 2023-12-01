// See https://aka.ms/new-console-template for more information

using System.Reflection;
using AdventOfCode2023.Day1;
var d1 = new Day1();

var lines = File.ReadAllText("./Day1/input.txt").Split("\n").ToList();
var result = d1.Part1(lines);
var result2 = d1.Part2(lines);

Console.WriteLine(result);
Console.WriteLine(result2);
