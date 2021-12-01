---
title: Syllabus
layout: single
toc: true
toc_label: "Syllabus Contents"
---

## Purpose and Objectives

This course introduces programming languages concepts with a hands-on,
"learn by doing" approach. This means both writing working programs
and understanding programming languages' behaviors by implementing
those behaviors in interpreters. Through this we will understand both
the commonalities between languages and the impact of various
language-design decisions. To summarize, in this course you will:

After this course you will know how to:


  1. Learn and utilize central PL concepts such as evaluation, scope,
     binding, and defunctionalization.
  1. Implement interpreters and extend them to evaluate new syntactic
     forms.
  1. Apply correctness-preserving program transformation techniques to
     guarantee program properties.
  1. Evaluate and critique the impact of language design decisions.

*This course is fast paced.* The course's material accumulates,
building to a must-pass project due toward the end of the course.
Weekly programming assignments help situate lectures and provide
opportunities to engage and practice concepts prior to the exams.
Grades from these assignments give you early feedback on your
understanding and performance. Further, these assignments ensure your
final grade will not be solely determined by your project and exams.
These will be discussed in detail below.

This syllabus contains policies and expectations I have established
for CS4400. Please read carefully the entire syllabus before
continuing in this course. I intend for these policies and
expectations to create a productive learning atmosphere for all
students. Unless you are prepared to abide by these policies and
expectations, you risk losing the opportunity to participate further
in the course. Policies and expectations as set forth in this syllabus
may be modified at any time by the course instructor. Notice of such
changes will be made by announcement in class, by written or email
notice, or by changes to this syllabus posted on the course website.

## Contact

The best way to get in contact for personal, private (FERPA, etc)
messages is via my email address
[jhemann@northeastern.edu](mailto:jhemann@northeastern.edu). You
should expect a response within 48 hours. You will find that I am
faster with [Piazza](piazza) or our public forums. If I deem it even
potentially useful to others, I will likely anonymize your letter,
re-post it on Piazza, answer it there, and forward you the link.

A great regular way to reach out for help is via our [office
hours]({{ site.baseurl }}/office-hours/).

## Grade Breakdown

I will assign overall course grades as follows:

     | Category                  | Weight (%) |
     |---------------------------+------------|
     | Lecture Quizzes           |         20 |
     | Exams                     |         35 |
     | Homework*                 |         45 |
     | TRACE                     |          1 | 
     | Total                     |        101 |

We (I) will calculate overall numeric grade according to the
[Northeastern grading schema]({{ site.baseurl
}}/assets/images/northeastern-grading-schema.jpg). Your final grades
will be at least as good as the standard calculation for As, Bs, Cs,
etc. To assign final letter grades at the end of the term I sum and
chart the numeric scores, and break students' grades where we see
inflection points (with no students' grade worse than their raw
calculation). This means I *cannot* give you a more precise estimate
of your grade than what you calculate from the raw score.

I will base some portions of your homework and lecture quiz
grades on completion and submission of the relevant exercises. I will
base the remaining portion of each on correctness. 


### Total Running Grade Calculation

We will track the completion portions of your lab and homework grades,
as well as your project grades, on
[Canvas](https://canvas.northeastern.edu/). You will have an
approximate assessment of your *current* grade status before the
Add/Drop deadline. We cannot give you a standing completion percentage
of the participation component of your grade because, logically, there
[are no pop
quizzes](https://en.wikipedia.org/wiki/Unexpected_hanging_paradox).

### Collaborative Course Construction and Feedback

I *want* you all's feedback and input. I am open to suggestions and
changes; I consider this preliminary until the end of the first week
of class. You can see any and all changes on the course website's
repository. I cannot promise that we will act on all suggestions, and
even those we find compelling may not be implementable as we go.

## Participation

I expect you to attend each lecture. We will [not take
attendance](https://vm.tiktok.com/ZMJBoQovg/) as such, but attendance
is a prerequisite for participation, a substantial portion of your
grade for this course. I expect students to attend every class and
remain in class throughout the duration of the session. Your absence
or tardiness will impact your ability to achieve course objectives
which could hurt your course grade. An absence, excused or unexcused,
does not relieve a student of any course requirement. Lecture content
quizzes serve as proxies for participation and thus attendance, as
well as to gauge students' understanding.

### Lecture Quizzes/Polls

Expect to have regular content quizzes during or toward the end of
lecture. These act as a forcing function, encouraging attention to
lectures and/or the ancillary readings and to alert me to students'
difficulties. Sometimes we take these for completion, others for
accuracy. These lecture quizzes belongs under your participation
grade. To account for illnesses, other commitments that come up, and
all the other vagaries of life, I'll drop 5/25 of these quizzes for
you all. I don't know that I'd be comfortable dropping 1/5 of them
under normal circumstances, but at least in the current situation that
seems like a reasonable precaution. If you think you're sick, please
do get yourself checked and be safe.


## Homework

Homework, consisting of weekly programming assignments, is an
essential part of the course. Assignments are available by at least
Thursday evening, and unless otherwise indicated, homework submissions
are due by 10 p.m. on the Wednesday after I have assigned them. We
give a few (randomly varying) extra minutes to account for network
time disagreements, but beyond this homework assignments are due
strictly on the day and time listed on the assignment.

To universally, uniformly and preemptively account for any number of
situations that arise, *I will drop every student's lowest homework
assignment grade.* You will also have an opportunity for a bonus
assignment to replace your lowest remaining assignment. This
absolution for one assignment and optional bonus *is* our late/etc
homework clemency; but for exceptional circumstances, I **shall not**
accept late homework otherwise; we are, however happy to go over these
missed submissions with you at office hours.

You will have ten regular homework assignments plus that one bonus
assignment; I will make the bonus assignment available between
Homework 9 and Homework 10.

You should make every effort to complete and submit each assignment.
If you are struggling with an assignment, it best to turn in what you
can complete and to seek help. Homework assignments will build on one
another conceptually, and some later assignments require the
successful completion of problems from earlier ones. **Do not fall
behind.** If you feel yourself falling behind, seek help immediately
and take advantage of office hours, your classmates, ancillary
readings, and additional support. Follow both the general homework
guidelines, as well as any special instructions given on the
assignment itself.

We allow an unlimited number of submissions per assignment, up to the
deadline.

We will evaluate your work both for correctness and for style. We
provide you an autograder test suite for each assignment. This
autograder merely ensures that your programs compute the correct
answers. Our homework problems are as much about _how_ you solve the
problems as _that_ you solve them. You should use the suggested style
as demonstrated in class. We are teaching, especially early in the
course, a particular way of thinking through and solving problems.
This technique generalizes to the course's subsequent, larger
problems, and programs in this style serve as input for program
transformation techniques we will learn. We will also make a holistic,
subjective evaluation of your submission under the [following
rubric]({{ site.baseurl }}/assets/images/grading_flowchart.png).

You should aim for a grade of 7 or better on homework assignments. If
you receive a lower grade, make sure to carefully revisit that
assignment before the next exam. I encourage you to contact an
instructor for help, even on prior assignments.

Homework assignments will almost always contain extra "just dessert"
problems. These are not required and do not impact your homework
grade--they are there for your fun and excitement. Often, these
problems explore interesting topics we will not have get to in class,
but we would be remiss in skipping them entirely.

Homeworks sets will almost always contain some brainteaster questions.
These are merely optional for those who want something extra to play
with. Often these extra problems show something interesting that we
haven't had the time to talk about in class. Usually there'll be some
dessert problems as well. These are also optional, and vary from
suuuper tricky to things we haven't yet solved to our satisfaction.


### Exams 

You will have two long-form in-class exams. We calibrate our exams to
make full use of the grading spectrum, and assume you fully understand
the lecture material and the skills and techniques practiced on
homework, and that you can apply them in new ways. They are not
intended to be "easy" for even accomplished students. The instructor
is known for exams that use the full width of the grading curve; this
is deliberate, as the common 30-point standard deviation is
insufficiently granular. We will provide you a practice exam, and
before each exam we will use the full period for an exam review
session. Exams offer bonus questions that can help improve your score,
potentially above 100 points.

### TRACE evaluations

I encourage students to take time and submit TRACE evaluations. Your
time is busy at the end of the term when these are available. In order
to fairly compensate you for that time without violating the integrity
or anonymity of the TRACE system, if 85% or more of the enrolled
students complete these TRACE evaluations, then I shall add a point
onto the class-wide final average.

## Project 

This course culminates in a final project. This project also presumes
an understanding of the prior material in the course, and acts as a
final test of your ability to understand and apply this material. *To
complete this assignment you must: 1) submit a working version of your
interpreter written in C 2) keep copies of your program at each stage
of this transformation 3) successfully pass a code review, to be
scheduled with one of the instructors.* We will provide a tool to
schedule a code review with an instructor. You may take as many code
reviews as you need, without penalty. This project is given as an
assignment, and graded for completion---you will receive 100\% if you
complete it successfully by the scheduled time. However, you must
complete all portions of this assignment before the date and time of
the final exam. *You must complete this assignment to receive a
passing grade in this course.*

## Lecture

The vast majority of course content will come from in-class lecture,
supplemented with notes distributed online. Therefore, attending
lecture is of the utmost importance. *You should make every effort to
attend each lecture, and take vigorous notes.* We will often provide
directly the answers to homework problems in lecture, and this course
is significantly more difficult for the student who misses one or more
lectures. There are no substitutes for participating in in-class
activities. We will sometimes distribute electronic transcripts of the
in-class code, but this is no substitute for careful notes and
understanding its development. I will not be taking attendance except
for lecture content quizzes. Regular class attendance is a student's
obligation, as is responsibility for all the content of class
meetings, including tests.

You should plan to have with you tools to take vigorous notes. Pencil
and paper, or some electronic tablet version of the aforementioned,
are especially effective. I have traditionally found the use of
laptops and cell phones in the classroom disruptive. I have come to
understand that I may be a bit of a Luddite and for the first time I'm
removing the usual prohibition. However, if students' laptops and cell
phones become disruptive for the general classroom environment or
students around them, I will revisit this decision. The use of cell
phones, smart phones, or other mobile communication devices is
disruptive, and is therefore prohibited during class.

I do not permit electronic video and/or audio recording of class
without prior permission. Unless the student obtains permission from
the instructor electronic video and/or audio recording of class is
prohibited. If you receive permission, any distribution of the
recording is prohibited. Students with specific electronic recording
accommodations authorized by the [DRC](academic-accommodations) do not
require instructor permission; however, the instructor must be
notified of any such accommodation prior to recording. Any
distribution of such recordings is prohibited. Obviously I cannot
[stop you](https://obsproject.com/), but it's to both our benefits.

## Additional Support 

In addition to lecture, we provide the following additional resources
for students to avail themselves. Do consider taking regular advantage
of them.

### Scheduled Office Hours

Course personnel will make ourselves available for 4-6 hours of
[office hours]({{ site.baseurl}}/office-hours/) available weekly,
concentrated toward assignment due dates. If our office hours schedule
in particularly ill-suited to your class schedule, let me know and we
may be able to adjust them. As per current university guidance, we
will hold these office hours remotely.

### Gradescope

We will be using Gradescope this term, which allows us to provide fast
and accurate feedback on your work. Homework will be submitted through
Gradescope, and homework and exam grades will be returned through
Gradescope. As soon as grades are posted, you will be notified
immediately so that you can log in and see your feedback. For
clarification on grades, please meet with your grader during office
hours.

To access Gradescope, click the Log In button on the Gradescope
website and enter your university email. Then enter your existing
password if you have one or click Forgot Password to reset it or
create one for the first time.


### Piazza Forums

Outside of office hours, you should utilize the class's Piazza forums
for questions. We have disabled private messages to instructors, but
you can choose to remain anonymous to the class when asking
questions. We prefer Piazza over email, as it gives other students the
opportunity to learn from those same anwers. Please restrict your
questions to those that do not ``give away the punchline'' to a
homework question. For more sensitive questions, or administrative
issues that should addressed in private, please email me at the
address listed on the front of this syllabus.

### Optional Texts 

There are *no required texts* for this class. Simply put, no companion
textbooks mirror this course's development of these topics. For
students who prefer to have readings to supplement lecture material,
we suggest the following two optional texts, which we refer to as SAOP
and EOPL. Each covers portions of material from this course, and
together the two texts contain supplementary material for almost every
lecture. The schedule lists optional readings from each when they
contain supplementary material that complements that lecture. Both are
available at the campus bookstore.

  - "Scheme and the Art of Programming" (Selections made available on this website)
  - ["Essentials of Programming Languages", 3rd edition](https://onesearch.library.northeastern.edu/permalink/f/365rt0/NEU_ALMA51224732410001401)

Neither will cover topics precisely the way we do in lecture. Their
implementations will also differ in technical details and particulars
of syntax. However, both provide material that translates to what we
do in class. Some students may choose to use these readings after
lectures to supplement their understanding. Especially diligent
students may use them to preview the lecture and be that much farther
ahead. In the rare circumstance you must miss lecture, these may help
supplement your understanding from lab, our additional support, and
perhaps lecture notes from a friend.

## Academic Integrity Policy

Students of course play an integral part in ensuring they receive the
full benefit of their coursework. The students of 4400 are certainly
beholden to the academic integrity policies of [Northeastern
University](http://www.northeastern.edu/osccr/academic-integrity-policy/)
and as laid out in the [student
handbook](https://cpb-us-w2.wpmucdn.com/sites.northeastern.edu/dist/8/569/files/2020/09/2021-22-UG-Student-Handbook.pdf),
the [Khoury
College](https://www.khoury.northeastern.edu/information-for-overview/current-undergrad/undergraduate-advising-academic-support/academic-policies/).

## Equity and Compliance

One of our responsibilities in supporting student learning 360° is to
help create a safe learning environment both in person and
virtually. You should carefully consult the university's [relevant
policies](https://www.northeastern.edu/ouec/title-ix-policy-2/), and
if you have or experience any violations of the above I encourage you
to take full advantage of the [university
resources](https://www.northeastern.edu/ouec/resources/main-campus-resources/).

It is also important that you know that federal regulations and
University policy require me to promptly convey any information about
certain kinds of misconduct known to me to our Deputy Title IX
Coordinator or IU’s Title IX Coordinator. In that event, they will
work with a small number of others on campus to ensure that
appropriate measures are taken and resources are made available to the
student who may have been harmed. Protecting a student’s privacy is of
utmost concern, and all involved will only share information with
those that need to know to ensure the University can respond and
assist. 

## Academic Accommodations 

If you have accommodations from the [Disability Resource Center
(DRC)](http://www.northeastern.edu/drc/) please submit your Professor
Notification Letter to me by email, preferably within the first two
weeks of the quarter, so I can do my part to help you achieve equal
access in this course. I am eager to discuss ways we can ensure your
full participation.

I encourage all students who may benefit from learning more about DRC
services to [contact the
DRC](http://www.northeastern.edu/drc/#fp-blog-entry:~:text=Contact%20Information).

## Technology and Platforms

We will use a variety of tools and platforms to facilitate teaching
and learning over the semester. These include Racket, Khoury Office
Hours, and Piazza. Please see the [technology page]({{ site.baseurl
}}/tech/) for more details.

## Acknowledgments 

I derived many of our course's lecture's contents and topics, as well
as assignments, from [Dan
Friedman](https://legacy.cs.indiana.edu/~dfried/)'s 311 at Indiana
University. We use [Krishnamurthi's `gradescope-racket` autograde
framework](https://github.com/shriram/gradescope-racket). [Lindsey
Kuper](https://users.soe.ucsc.edu/~lkuper/) inspires some of this site
as well as being all-around inspirational.

![In the syllabus]({{ site.baseurl }}/assets/images/syllabus.gif "Might just be worth checking.")
