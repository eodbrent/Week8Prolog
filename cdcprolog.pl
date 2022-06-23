/*
Definitions:
Cardinal (Primary) Symptoms: cough, fever (> 100.0° F) , shortness of breath (that interferes with activities), 
    loss of sense of taste, loss of sense of smell.

Secondary Symptoms: fatigue, muscle aches, headache, sore throat, congestion, runny node, nausea, vomiting, diarrhea.

Low-risk Exposure: exposure to a symptomatic person while wearing Personal Protective Equipment (PPE) or a 
    brief exchange without PPE (see below) and no direct contact to secretions

High-Risk Exposure: Close contact exposure to respiratory droplets (coughing/sneezing) with symptomatic 
    person where neither party was wearing a surgical mask; direct contact with infected secretions, 
    close contact 3-6 feet with symptomatic person, or greater than 15 minutes contact with symptomatic person.

Testing, Quarantine, and Isolation Guidelines

If a patient has at least one cardinal or two secondary systems, then they should be tested for Covid-19.
   If the test or result is unknown or pending, then
     Patient should isolate for a minimum of 10 days from symptom onset AND symptoms improving, AND no fever for 24 hours
   else (negative test)
     Patient should isolate until symptoms improving AND no fever for 24 hours (i.e. no mandatory 10-day isolation)
else (patients is asymptomatic)
    Patient should isolate for 10 days from the date of the test

If Patient has had a high-risk exposure
  if Exposed patient is asymptomatic then
     Patient should remain under home quarantine for 14 days from date of high-risk exposure
  else if exposed patient develops symptoms and test positive then
     Patient should isolate for a minimum of 10 days from symptom onset AND symptoms improving AND no fever for 24 hours.
  else if exposed patient develops symptoms and test negative then
     Patient should isolate until symptoms improving AND no fever for 24 hours AND 14 days have passed since the high-risk exposure
  else exposure source tests negative
      if asymptomatic, patient can end quarantine   
else  (patient has had a low-risk exposure)
    No quarantine is necessary. Patient should self-monitor for symptoms for 14 days from date of the exposure.

Requirements: Sketch out a few Prolog facts, rules, and a query that could be used as the basis for a 
Prolog software application that assists a Physician in making a recommendations to patients using the 
above CDC guidelines. You don't have to be a medical expert to complete this assignment. You do not 
have to implement this program, but try to get the Prolog correct with reasonable accurate (syntax and semantics).
*/
:- use_module(library(clpfd)).
primary(cough).
primary(fever).
primary(short_breath).
primary(loss_taste).
primary(loss_smell).

secondary(fatigue).
secondary(muscle_aches).
secondary(headache).
secondary(sore_throat).
secondary(congestion).
secondary(runny_nose).
secondary(nausea).
secondary(vomiting).
secondary(diarrhea).

human(patient).

query :- writeln('Primary Symptoms:'), findall(Primary, primary(Primary), Primaries), maplist(writeln, Primaries), 
    writeln('Secondary Symptoms:'), findall(Secondary, secondary(Secondary), Secondaries), maplist(writeln, Secondaries),
    writeln('How many primary symptoms is the patient experiencing?'),
    read(P),
    writeln('How many secondary symptoms is the patient experience?'),
    read(S),
    test_procedure(P,S).

% P = number of primary/cardinal symptoms, S is secondary
test_procedure(P, S) :-
       (P =:= 0, S < 2 -> writeln('No need to test patient') ;
        P > 0 -> writeln('Patient should be tested') ;
        S > 1 -> writeln('Patient should be tested')).
% test results 0 = negative, 1 = positive, 2 = pending.
test_results(N) :-
    (N #= 0, write('Patient should isolate until symptoms improve or no fever for 25 hours.')) ;
    (N #= 1, write('Patient should isolate for 10 days from date of test.')) ;
    (N #= 2, write('Patient should isolate for minimum of 10 days from symptom onset and symptoms improving, and no fever for 24 hours.')).


/*
if patient had highrisk exposure
    if exposed patient is asymptomatic
        home quarantine for 14 days from date of Exposure
    else if develops symptoms and tests positive
        isolate for minimum of 10 days and no fever for 24 hours
    else if develops systems and test negative
        isolate until symptoms improve and no fever for 24 hours
    else exposure source tests negative
        and asymptomatic, patient can end quarantine
else patient has had low risk exposure
    no quarantine - self monitor */