-- Notation convention :

Scenario Big Four Winds - Example 1
Given concealed we,we,we ws,ws,ws,ws ww,ww,ww wn,wn,wn dr,dr
Ginven prevalent wind is East
Given player wind is North
When scoring
Then BigFourWinds is scored
Then AllHonors is scored
Then AllPungs is not scored
Then PrevalentWind is not scored
Then SeatWind is not scored
Then PungOfTerminalsOrHonors is not scored

Scenario Big Four Winds - Example 2
Given concealed we,we,we ws,ws,ws,ws ww,ww,ww wn,wn,wn c9,c9
When scoring
Then BigFourWinds is scored
Then AllTerminalsAndHonors is scored
Then HalfFlush is scored
Then AllPungs is not scored
Then PrevalentWind is not scored
Then SeatWind is not scored
Then PungOfTerminalsOrHonors is not scored

Scenario Big Four Winds - Example 3
Given concealed we,we,we ws,ws,ws,ws ww,ww,ww wn,wn,wn b3,b3
When scoring
Then BigFourWinds is scored
Then AllTerminalsAndHonors is scored
Then HalfFlush is scored
Then AllPungs is not scored
Then PrevalentWind is not scored
Then SeatWind is not scored
Then PungOfTerminalsOrHonors is not scored