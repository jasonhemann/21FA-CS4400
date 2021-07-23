---
title: I have a timestamp showing the unmodified file! Will you accept my late work?
date: 2021-10-27
---


No. It's not a question of me believing you or not. I've never been
able to use timestamps on files. I keep on my laptop hard drive a text
file "dated" to the date and time of the sinking of the Titanic (I
mean the one from 1912, not 1997). Here is how I got it:

```bash
[jhemann:~] 14:55:29$ SetFile -d "04/15/1912 02:20:00" ~/SinkingOfTheTitanic.txt
[jhemann:~] 14:56:19$ stat ~/SinkingOfTheTitanic.txt
16777221 22202324 -rw-r--r-- 1 jhemann staff 0 49 "Sep 27 14:56:19 2021" "Apr 15 02:20:00 1912" "Sep 27 14:56:19 2021" "May 21 09:48:16 2048" 4096 8 0 /Users/jhemann/SinkingOfTheTitanic.txt
```

As you can see, timestamps are unreliable. At base, these are all
mutable bits on machines over which we have full control. Of course
our usual [uniform amnesty for late work]({{ site.baseurl
}}/FAQ/late-work/) applies to you in this instance as well. 

