#lang scribble/manual

@title{Motivation}

HTML is notoriously time-consuming and annoying to work with. With HML, we sought to ease the process
of extracting data from HTML through the use of HTML matchers. Matchers allow for robust and easy
destructuring of HTML documents.

The key strengths of these patterns are due to there loose coupling to the underlying document that
is being matched. Patterns are made out of compisition of smaller patterns, and the library will
do some "spelunking" through the document in order to identify matching sub documents.

Low dependency on the structure of underlying HTML documents allows for reuse of common matching
patterns as well as decreasing the likelihood a change to a webpage will break the code.