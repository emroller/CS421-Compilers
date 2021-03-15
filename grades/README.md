You will find four files in this directory:

- `grades/specs.txt` - This is the list of learing modules, outcomes, and assessments that fulfill them.
- `grades/grades.txt` - This is the same list, but with your scores and how many LM points you got from them.
- `grades/updates.txt` - Announcements about the current grade reports to go out.  You should read this each time there's an update.
                         This is where we'll say things like "Exam 2 score are 50% added now" or "We just added MP 5 scores".
                         Kind of like a git commit message.
- `grades/README.md` - Instructions on how to read these files.

## How to read `specs.txt`

Here is part of the `specs.txt`:


```
Learning Module               Outcome (LM points)                                              Assessment (LM points)               
----------------------------- ---------------------------------------------------------------- -------------------------------------
1. Recursion                  1. Write a recursive function over integers (2)                  Recursion Activity problem 1 (2)     
                              2. Write a mapping recursive function over lists (2)             Recursion Activity problem 2 (2)     
                              3. Write a folding recursive function over lists (2)             Recursion Activity problem 3 (2)     
                              4. Write a tail recursive function (2)                           Exam 1 - Tail Recursion (2)          
                              5. Indicate an advantage of tail recursion over direct style (2) Exam 1 - Tail Recursion Advantage (2)
----------------------------- ---------------------------------------------------------------- -------------------------------------
2. Algebraic Data Types       1. Write a datatype declaration (2)                              MP 1 (2)                             
                              2. Consume a recursive datatype (2)                              ADT Activity (2)                     
                              3. Transform a recursive datatype (3)                            Exam 1 - Algebraic Data Types (3)    
                              4. Produce a recursive datatype (3)                              MP 1 (3)                             
----------------------------- ---------------------------------------------------------------- -------------------------------------
3. Higher Order Functions     1. Using HOFs (5)                                                Exam 1 - Using HOFs (5)              
                              2. Writing HOFs (5)                                              Exam 1 - Writing HOFs (3)            
                                                                                               HOF Activity (2)
----------------------------- ---------------------------------------------------------------- -------------------------------------
4. Interpreters (Expressions) 1. Write data type for eval (2)                                  MP 2 (2)                             
                              2. Write a lifting function (4)                                  MP 2 (4)                             
                              3. Write an eval clause from semantics (4)                       MP 2 (4)                             
----------------------------- ---------------------------------------------------------------- -------------------------------------
5. Interpreters (Functions)   1. Use eval to create a closure (4)                              MP 2 (4)                             
                              2. Use eval to apply a function (4)                              MP 2 (4)                             
                              3. Show why closure environment is needed. (2)                   Exam 2 - Why Closure Environment (2) 
                              4. Show difference between procedure and function (2)            Exam 2 - Procedure vs Function (2)   
```

The are three columns:
 - Learning Module
 - Outcome, which lists the outcome title and how many points it contributes to the LM.
 - Assessment, which lists the assessment title and how many points it contributes to an outcome.

Things that can and will happen:
 - A single assessment may fulfill more than one outcome.  E.g., MP 2 with LM 4.
 - A single outcome may have more than one assessment attached to it.  E.g., Outcome 3.2
 - A group of assessments for an outcome may add up to more points than the outcome is worth.  In such case, the total number of LM
   points is limited to the outcome points.  (This allows us to give more than one chance to master an outcome.)

Here are some lines from a made up `grades.txt`

```
Learning Module               Outcome (LM points)                                              Assessment (LM points)            Score       
----------------------------- ---------------------------------------------------------------- --------------------------------- ------------
1. Recursion                  1. Write a recursive function over integers (2)                  Recursion Activity problem 1      100.0 (2.00)
                              5. Indicate an advantage of tail recursion over direct style (2) Exam 1 - Tail Recursion Advantage 80.0 (1.60) 
----------------------------- ---------------------------------------------------------------- --------------------------------- ------------
3. Higher Order Functions     1. Using HOFs (5)                                                Exam 1 - Using HOFs (version 2)   100.0 (5.00)
                              2. Writing HOFs (5)                                              Exam 1 - Writing HOFs (version 1) 100.0 (3.00)
                                                                                               Fixpoint Activity                 x 50.0 (0)   
----------------------------- ---------------------------------------------------------------- --------------------------------- ------------
5. Interpreters (Functions)   1. Use eval to create a closure (4)                              MP 2                              100.0 (4.00)
                              2. Use eval to apply a function (4)                              MP 2                              100.0 (4.00)
                              3. Show why closure environment is needed. (2)                                                                 
                              4. Show difference between procedure and function (2)                                                          
```

The `Recursion Activity problem 1` line has a score column of `100.0 (2.00)`.  This means the score for that assessment was 100%.
(We will normalize all scores to percentages.)  The 2.00 means they got 2 LM points.

The `Exam 1 - Tail Recursion Advantage` line has a score column of `80.0 (1.60)`.  80% is the threshold for getting LM points, and we
multiply the percentage, hence the 1.6 LM points.  Originally we only wanted to count things that got 100%, but the way Prairielearn handles
late things is by decreasing the score, and there's no indication that it was late.  It's hard to do well enough to get 80% on something
and not get 100% on something anyway. :)

The `Fixpoint Activity` line has a score column of `x 50.0 (0)`.  The score of 50% is below the threshold, so no LM points are earned, and we
indicate this situation with an `x`.
