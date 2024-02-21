open Robots

let B1 = Board()
let B2 = Robot(5,3,"CC")
let B3 = Robot(15, 15, "BB")
let B4 = Robot (1,1, "AA")
B1.AddElement (BoardFrame(15,15))
B1.AddElement (Goal(7,5))
B1.AddElement (B2)
B1.AddElement (B3)
B1.AddElement (B4)
B1.AddElement (VerticalWall(5,5,3))
B1.AddElement (HorizontalWall(6,13,2))
B1.AddElement (Trampoline(7, 14))
B1.AddElement (Mirror(1,3))
B1.AddRobot(B2)
B1.AddRobot(B3)
B1.AddRobot (B4)
let B5 = B1.Robots
let B6 = Game(B1, B5)
B6.Play()