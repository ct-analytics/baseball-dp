% This program computes the win probability and optimal strategy at each state
% in a particular half inning, taking as given the boundary conditions W(d,leadoff,ondeck)
% for that half inning.
% Copyright 2004 William S. Krasker

b0=1;
b1=2;
b2=3;
b3=4;
b12=5;
b13=6;
b23=7;
b123=8;

outs0=1;
outs1=2;
outs2=3;

nobody=1;

a=zeros(11,1);

V=zeros(9,dtop,3,8,9,9);
strategy=ones(9,dtop,3,8,9,9);

for m=1:3
for n=1:8
for batter=1:9
for onfirst=1:9
for leadoff=1:9
   V(batter,dtop,m,n,onfirst,leadoff)=1;
end
end
end
end
end


for d=dtop-1:-1:1

%                                       2 outs, bases loaded

for batter=1:9
   if batter == 9
      ondeck=1;
   else
      ondeck=batter+1;
   end
for onfirst=1:9
for leadoff=1:9

outs=outs2;
a(1)=W(d,leadoff,ondeck);                                     %strike out or short fly out
a(2)=a(1);                                                    %long fly out
a(3)=a(1);                                                    %hard ground out
a(4)=a(1);                                                    %soft ground out
a(5 )=V(ondeck,min(dtop,d+1),outs,b123,batter,leadoff);       %walk
a(6 )=V(ondeck,min(dtop,d+2),outs,b12 ,batter,leadoff);       %short single
a(7 )=V(ondeck,min(dtop,d+2),outs,b13 ,batter,leadoff);       %long single
a(8 )=V(ondeck,min(dtop,d+2),outs,b23 ,nobody,leadoff);       %short double
a(9 )=V(ondeck,min(dtop,d+3),outs,b2  ,nobody,leadoff);       %long double
a(10)=V(ondeck,min(dtop,d+3),outs,b3  ,nobody,leadoff);       %triple
a(11)=V(ondeck,min(dtop,d+4),outs,b0  ,nobody,leadoff);       %home run
walk=a(5);
hitaway=probs(batter,:)*a;
V(batter,d,outs,b123,onfirst,leadoff)=min(hitaway,walk);
if V(batter,d,outs,b123,onfirst,leadoff)==walk
   strategy(batter,d,outs,b123,onfirst,leadoff)=4;
end

end
end
end

%                                       2 outs, men on 2nd and 3rd

for batter=1:9
   if batter == 9
      ondeck=1;
   else
      ondeck=batter+1;
   end
for onfirst=1:9
for leadoff=1:9

outs=outs2;
a(1)=W(d,leadoff,ondeck);                                     %strike out or short fly out
a(2)=a(1);                                                    %long fly out
a(3)=a(1);                                                    %hard ground out
a(4)=a(1);                                                    %soft ground out
a(5 )=V(ondeck,d            ,outs,b123,batter,leadoff);       %walk
a(6 )=V(ondeck,min(dtop,d+2),outs,b1  ,batter,leadoff);       %short single
a(7 )=V(ondeck,min(dtop,d+2),outs,b1  ,batter,leadoff);       %long single
a(8 )=V(ondeck,min(dtop,d+2),outs,b2  ,nobody,leadoff);       %short double
a(9 )=V(ondeck,min(dtop,d+2),outs,b2  ,nobody,leadoff);       %long double
a(10)=V(ondeck,min(dtop,d+2),outs,b3  ,nobody,leadoff);       %triple
a(11)=V(ondeck,min(dtop,d+3),outs,b0  ,nobody,leadoff);       %home run
walk=a(5);
hitaway=probs(batter,:)*a;
V(batter,d,outs,b23,onfirst,leadoff)=min(hitaway,walk);
if V(batter,d,outs,b23,onfirst,leadoff)==walk
   strategy(batter,d,outs,b23,onfirst,leadoff)=4;
end

end
end
end

%                                       2 outs, men on 1st and 3rd

for batter=1:9
   if batter == 9
      ondeck=1;
   else
      ondeck=batter+1;
   end
for onfirst=1:9
for leadoff=1:9

outs=outs2;
a(1)=W(d,leadoff,ondeck);                                     %strike out or short fly out
a(2)=a(1);                                                    %long fly out
a(3)=a(1);                                                    %hard ground out
a(4)=a(1);                                                    %soft ground out
a(5 )=V(ondeck,d            ,outs,b123,batter,leadoff);       %walk
a(6 )=V(ondeck,min(dtop,d+1),outs,b12 ,batter,leadoff);       %short single
a(7 )=V(ondeck,min(dtop,d+1),outs,b13 ,batter,leadoff);       %long single
a(8 )=V(ondeck,min(dtop,d+1),outs,b23 ,nobody,leadoff);       %short double
a(9 )=V(ondeck,min(dtop,d+2),outs,b2  ,nobody,leadoff);       %long double
a(10)=V(ondeck,min(dtop,d+2),outs,b3  ,nobody,leadoff);       %triple
a(11)=V(ondeck,min(dtop,d+3),outs,b0  ,nobody,leadoff);       %home run
walk=a(5);
hitaway=probs(batter,:)*a;
steal=psteal(onfirst)*V(batter,d,outs,b23,nobody,leadoff) ...
                 +(1-psteal(onfirst))*W(d,leadoff,batter);
V(batter,d,outs,b13,onfirst,leadoff)=min(max(hitaway,steal),walk);
if V(batter,d,outs,b13,onfirst,leadoff)==walk
   strategy(batter,d,outs,b13,onfirst,leadoff)=4;
elseif V(batter,d,outs,b13,onfirst,leadoff)==steal 
   strategy(batter,d,outs,b13,onfirst,leadoff)=2; 
end

end
end
end

%                                       2 outs, men on 1st and 2nd

for batter=1:9
   if batter == 9
      ondeck=1;
   else
      ondeck=batter+1;
   end
for onfirst=1:9
for leadoff=1:9

outs=outs2;
a(1)=W(d,leadoff,ondeck);                                     %strike out or short fly out
a(2)=a(1);                                                    %long fly out
a(3)=a(1);                                                    %hard ground out
a(4)=a(1);                                                    %soft ground out
a(5 )=V(ondeck,d            ,outs,b123,batter,leadoff);       %walk
a(6 )=V(ondeck,min(dtop,d+1),outs,b12 ,batter,leadoff);       %short single
a(7 )=V(ondeck,min(dtop,d+1),outs,b13 ,batter,leadoff);       %long single
a(8 )=V(ondeck,min(dtop,d+1),outs,b23 ,nobody,leadoff);       %short double
a(9 )=V(ondeck,min(dtop,d+2),outs,b2  ,nobody,leadoff);       %long double
a(10)=V(ondeck,min(dtop,d+2),outs,b3  ,nobody,leadoff);       %triple
a(11)=V(ondeck,min(dtop,d+3),outs,b0  ,nobody,leadoff);       %home run
walk=a(5);
hitaway=probs(batter,:)*a;
V(batter,d,outs,b12,onfirst,leadoff)=min(hitaway,walk);
if V(batter,d,outs,b12,onfirst,leadoff)==walk
   strategy(batter,d,outs,b12,onfirst,leadoff)=4;
end

end
end
end

%                                       2 outs, man on 3rd

for batter=1:9
   if batter == 9
      ondeck=1;
   else
      ondeck=batter+1;
   end
for onfirst=1:9
for leadoff=1:9

outs=outs2;
a(1)=W(d,leadoff,ondeck);                                     %strike out or short fly out
a(2)=a(1);                                                    %long fly out
a(3)=a(1);                                                    %hard ground out
a(4)=a(1);                                                    %soft ground out
a(5 )=V(ondeck,d            ,outs,b13 ,batter,leadoff);       %walk
a(6 )=V(ondeck,min(dtop,d+1),outs,b1  ,batter,leadoff);       %short single
a(7 )=V(ondeck,min(dtop,d+1),outs,b1  ,batter,leadoff);       %long single
a(8 )=V(ondeck,min(dtop,d+1),outs,b2  ,nobody,leadoff);       %short double
a(9 )=V(ondeck,min(dtop,d+1),outs,b2  ,nobody,leadoff);       %long double
a(10)=V(ondeck,min(dtop,d+1),outs,b3  ,nobody,leadoff);       %triple
a(11)=V(ondeck,min(dtop,d+2),outs,b0  ,nobody,leadoff);       %home run
walk=a(5);
hitaway=probs(batter,:)*a;
V(batter,d,outs,b3,onfirst,leadoff)=min(hitaway,walk);
if V(batter,d,outs,b3,onfirst,leadoff)==walk
   strategy(batter,d,outs,b3,onfirst,leadoff)=4;
end

end
end
end

%                                       2 outs, man on 2nd

for batter=1:9
   if batter == 9
      ondeck=1;
   else
      ondeck=batter+1;
   end
for onfirst=1:9
for leadoff=1:9

outs=outs2;
a(1)=W(d,leadoff,ondeck);                                     %strike out or short fly out
a(2)=a(1);                                                    %long fly out
a(3)=a(1);                                                    %hard ground out
a(4)=a(1);                                                    %soft ground out
a(5 )=V(ondeck,d            ,outs,b12 ,batter,leadoff);       %walk
a(6 )=V(ondeck,min(dtop,d+1),outs,b1  ,batter,leadoff);       %short single
a(7 )=V(ondeck,min(dtop,d+1),outs,b1  ,batter,leadoff);       %long single
a(8 )=V(ondeck,min(dtop,d+1),outs,b2  ,nobody,leadoff);       %short double
a(9 )=V(ondeck,min(dtop,d+1),outs,b2  ,nobody,leadoff);       %long double
a(10)=V(ondeck,min(dtop,d+1),outs,b3  ,nobody,leadoff);       %triple
a(11)=V(ondeck,min(dtop,d+2),outs,b0  ,nobody,leadoff);       %home run
walk=a(5);
hitaway=probs(batter,:)*a;
V(batter,d,outs,b2,onfirst,leadoff)=min(hitaway,walk);
if V(batter,d,outs,b2,onfirst,leadoff)==walk
   strategy(batter,d,outs,b2,onfirst,leadoff)=4;
end

end
end
end

%                                       2 outs, man on 1st

for batter=1:9
   if batter == 9
      ondeck=1;
   else
      ondeck=batter+1;
   end
for onfirst=1:9
for leadoff=1:9

outs=outs2;
a(1)=W(d,leadoff,ondeck);                                     %strike out or short fly out
a(2)=a(1);                                                    %long fly out
a(3)=a(1);                                                    %hard ground out
a(4)=a(1);                                                    %soft ground out
a(5 )=V(ondeck,d            ,outs,b12 ,batter,leadoff);       %walk
a(6 )=V(ondeck,d            ,outs,b12 ,batter,leadoff);       %short single
a(7 )=V(ondeck,d            ,outs,b13 ,batter,leadoff);       %long single
a(8 )=V(ondeck,d            ,outs,b23 ,nobody,leadoff);       %short double
a(9 )=V(ondeck,min(dtop,d+1),outs,b2  ,nobody,leadoff);       %long double
a(10)=V(ondeck,min(dtop,d+1),outs,b3  ,nobody,leadoff);       %triple
a(11)=V(ondeck,min(dtop,d+2),outs,b0  ,nobody,leadoff);       %home run
walk=a(5);
hitaway=probs(batter,:)*a;
steal=psteal(onfirst)*V(batter,d,outs,b2,nobody,leadoff) ...
                 +(1-psteal(onfirst))*W(d,leadoff,batter);
V(batter,d,outs,b1,onfirst,leadoff)=min(max(hitaway,steal),walk);
if V(batter,d,outs,b1,onfirst,leadoff)==walk
   strategy(batter,d,outs,b1,onfirst,leadoff)=4;
elseif V(batter,d,outs,b1,onfirst,leadoff)==steal 
   strategy(batter,d,outs,b1,onfirst,leadoff)=2; 
end

end
end
end

%                                       2 outs, bases empty

for batter=1:9
   if batter == 9
      ondeck=1;
   else
      ondeck=batter+1;
   end
for onfirst=1:9
for leadoff=1:9

outs=outs2;
a(1)=W(d,leadoff,ondeck);                                     %strike out or short fly out
a(2)=a(1);                                                    %long fly out
a(3)=a(1);                                                    %hard ground out
a(4)=a(1);                                                    %soft ground out
a(5 )=V(ondeck,d            ,outs,b1  ,batter,leadoff);       %walk
a(6 )=V(ondeck,d            ,outs,b1  ,batter,leadoff);       %short single
a(7 )=V(ondeck,d            ,outs,b1  ,batter,leadoff);       %long single
a(8 )=V(ondeck,d            ,outs,b2  ,nobody,leadoff);       %short double
a(9 )=V(ondeck,d            ,outs,b2  ,nobody,leadoff);       %long double
a(10)=V(ondeck,d            ,outs,b3  ,nobody,leadoff);       %triple
a(11)=V(ondeck,min(dtop,d+1),outs,b0  ,nobody,leadoff);       %home run
walk=a(5);
hitaway=probs(batter,:)*a;
V(batter,d,outs,b0,onfirst,leadoff)=min(hitaway,walk);
if V(batter,d,outs,b0,onfirst,leadoff)==walk
   strategy(batter,d,outs,b0,onfirst,leadoff)=4;
end

end
end
end

%                                       1 out, bases loaded

for batter=1:9
   if batter == 9
      ondeck=1;
   else
      ondeck=batter+1;
   end
for onfirst=1:9
for leadoff=1:9

outs=outs1;
a(1 )=V(ondeck,d            ,outs2,b123,onfirst,leadoff);     %strike out or short fly out
a(2 )=V(ondeck,min(dtop,d+1),outs2,b12 ,onfirst,leadoff);     %long fly out
a(3 )=W(d,leadoff,ondeck);                                    %hard ground out
a(4 )=V(ondeck,d            ,outs2,b123,batter,leadoff);      %soft ground out
a(5 )=V(ondeck,min(dtop,d+1),outs,b123 ,batter,leadoff);      %walk
a(6 )=V(ondeck,min(dtop,d+2),outs,b12  ,batter,leadoff);      %short single
a(7 )=V(ondeck,min(dtop,d+2),outs,b13  ,batter,leadoff);      %long single
a(8 )=V(ondeck,min(dtop,d+2),outs,b23  ,nobody,leadoff);      %short double
a(9 )=V(ondeck,min(dtop,d+3),outs,b2   ,nobody,leadoff);      %long double
a(10)=V(ondeck,min(dtop,d+3),outs,b3   ,nobody,leadoff);      %triple
a(11)=V(ondeck,min(dtop,d+4),outs,b0   ,nobody,leadoff);      %home run
walk=a(5);
hitaway=probs(batter,:)*a;
V(batter,d,outs,b123,onfirst,leadoff)=min(hitaway,walk);
if V(batter,d,outs,b123,onfirst,leadoff)==walk
   strategy(batter,d,outs,b123,onfirst,leadoff)=4;
end

end
end
end

%                                       1 out, men on 2nd and 3rd

for batter=1:9
   if batter == 9
      ondeck=1;
   else
      ondeck=batter+1;
   end
for onfirst=1:9
for leadoff=1:9

outs=outs1;
a(1 )=V(ondeck,d            ,outs2,b23,nobody ,leadoff);      %strike out or short fly out
a(2 )=V(ondeck,min(dtop,d+1),outs2,b3 ,nobody ,leadoff);      %long fly out
a(3 )=a(1);                                                   %hard ground out
a(4 )=V(ondeck,min(dtop,d+1),outs2,b3 ,nobody,leadoff);       %soft ground out
a(5 )=V(ondeck,d            ,outs,b123,batter,leadoff);       %walk
a(6 )=V(ondeck,min(dtop,d+2),outs,b1  ,batter,leadoff);       %short single
a(7 )=V(ondeck,min(dtop,d+2),outs,b1  ,batter,leadoff);       %long single
a(8 )=V(ondeck,min(dtop,d+2),outs,b2  ,nobody,leadoff);       %short double
a(9 )=V(ondeck,min(dtop,d+2),outs,b2  ,nobody,leadoff);       %long double
a(10)=V(ondeck,min(dtop,d+2),outs,b3  ,nobody,leadoff);       %triple
a(11)=V(ondeck,min(dtop,d+3),outs,b0  ,nobody,leadoff);       %home run
walk=a(5);
hitaway=probs(batter,:)*a;
V(batter,d,outs,b23,onfirst,leadoff)=min(hitaway,walk);
if V(batter,d,outs,b23,onfirst,leadoff)==walk
   strategy(batter,d,outs,b23,onfirst,leadoff)=4;
end

end
end
end

%                                       1 out, men on 1st and 3rd

for batter=1:9
   if batter == 9
      ondeck=1;
   else
      ondeck=batter+1;
   end
for onfirst=1:9
for leadoff=1:9

outs=outs1;
a(1 )=V(ondeck,d            ,outs2,b13,onfirst,leadoff);      %strike out or short fly out
a(2 )=V(ondeck,min(dtop,d+1),outs2,b1 ,onfirst,leadoff);      %long fly out
a(3 )=W(d,leadoff,ondeck);                                    %hard ground out
a(4 )=V(ondeck,min(dtop,d+1),outs2,b2 ,nobody,leadoff);       %soft ground out
a(5 )=V(ondeck,d            ,outs,b123,batter,leadoff);       %walk
a(6 )=V(ondeck,min(dtop,d+1),outs,b12 ,batter,leadoff);       %short single
a(7 )=V(ondeck,min(dtop,d+1),outs,b13 ,batter,leadoff);       %long single
a(8 )=V(ondeck,min(dtop,d+1),outs,b23 ,nobody,leadoff);       %short double
a(9 )=V(ondeck,min(dtop,d+2),outs,b2  ,nobody,leadoff);       %long double
a(10)=V(ondeck,min(dtop,d+2),outs,b3  ,nobody,leadoff);       %triple
a(11)=V(ondeck,min(dtop,d+3),outs,b0  ,nobody,leadoff);       %home run
walk=a(5);
hitaway=probs(batter,:)*a;
steal=psteal(onfirst)*V(batter,d,outs,b23,nobody,leadoff) ...
                 +(1-psteal(onfirst))*V(batter,d,outs2,b3,nobody,leadoff);
V(batter,d,outs,b13,onfirst,leadoff)=min(max(hitaway,steal),walk);
if V(batter,d,outs,b13,onfirst,leadoff)==walk
   strategy(batter,d,outs,b13,onfirst,leadoff)=4;
elseif V(batter,d,outs,b13,onfirst,leadoff)==steal 
   strategy(batter,d,outs,b13,onfirst,leadoff)=2; 
end

end
end
end

%                                       1 out, men on 1st and 2nd

for batter=1:9
   if batter == 9
      ondeck=1;
   else
      ondeck=batter+1;
   end
for onfirst=1:9
for leadoff=1:9

outs=outs1;
a(1 )=V(ondeck,d            ,outs2,b12,onfirst,leadoff);      %strike out or short fly out
a(2 )=V(ondeck,d            ,outs2,b13,onfirst,leadoff);      %long fly out
a(3 )=W(d,leadoff,ondeck);                                    %hard ground out
a(4 )=V(ondeck,d            ,outs2,b12,batter,leadoff);       %soft ground out
a(5 )=V(ondeck,d            ,outs,b123,batter,leadoff);       %walk
a(6 )=V(ondeck,min(dtop,d+1),outs,b12 ,batter,leadoff);       %short single
a(7 )=V(ondeck,min(dtop,d+1),outs,b13 ,batter,leadoff);       %long single
a(8 )=V(ondeck,min(dtop,d+1),outs,b23 ,nobody,leadoff);       %short double
a(9 )=V(ondeck,min(dtop,d+2),outs,b2  ,nobody,leadoff);       %long double
a(10)=V(ondeck,min(dtop,d+2),outs,b3  ,nobody,leadoff);       %triple
a(11)=V(ondeck,min(dtop,d+3),outs,b0  ,nobody,leadoff);       %home run
walk=a(5);
bunt=sacbunt(batter)*V(ondeck,d,outs2,b23,nobody,leadoff) ...
       +(1-sacbunt(batter))*V(ondeck,d,outs2,b12,batter,leadoff);
hitaway=probs(batter,:)*a;
V(batter,d,outs,b12,onfirst,leadoff)=min(max(bunt,hitaway),walk);
if V(batter,d,outs,b12,onfirst,leadoff)==walk
   strategy(batter,d,outs,b12,onfirst,leadoff)=4;
elseif V(batter,d,outs,b12,onfirst,leadoff)==bunt 
   strategy(batter,d,outs,b12,onfirst,leadoff)=3; 
end

end
end
end

%                                       1 out, man on 3rd

for batter=1:9
   if batter == 9
      ondeck=1;
   else
      ondeck=batter+1;
   end
for onfirst=1:9
for leadoff=1:9

outs=outs1;
a(1 )=V(ondeck,d            ,outs2,b3 ,nobody ,leadoff);      %strike out or short fly out
a(2 )=V(ondeck,min(dtop,d+1),outs2,b0 ,nobody ,leadoff);      %long fly out
a(3 )=a(1);                                                   %hard ground out
a(4 )=a(2);                                                   %soft ground out
a(5 )=V(ondeck,d            ,outs,b13 ,batter,leadoff);       %walk
a(6 )=V(ondeck,min(dtop,d+1),outs,b1  ,batter,leadoff);       %short single
a(7 )=V(ondeck,min(dtop,d+1),outs,b1  ,batter,leadoff);       %long single
a(8 )=V(ondeck,min(dtop,d+1),outs,b2  ,nobody,leadoff);       %short double
a(9 )=V(ondeck,min(dtop,d+1),outs,b2  ,nobody,leadoff);       %long double
a(10)=V(ondeck,min(dtop,d+1),outs,b3  ,nobody,leadoff);       %triple
a(11)=V(ondeck,min(dtop,d+2),outs,b0  ,nobody,leadoff);       %home run
walk=a(5);
hitaway=probs(batter,:)*a;
V(batter,d,outs,b3,onfirst,leadoff)=min(hitaway,walk);
if V(batter,d,outs,b3,onfirst,leadoff)==walk
   strategy(batter,d,outs,b3,onfirst,leadoff)=4;
end

end
end
end

%                                       1 out, man on 2nd

for batter=1:9
   if batter == 9
      ondeck=1;
   else
      ondeck=batter+1;
   end
for onfirst=1:9
for leadoff=1:9

outs=outs1;
a(1 )=V(ondeck,d            ,outs2,b2 ,nobody ,leadoff);      %strike out or short fly out
a(2 )=V(ondeck,d            ,outs2,b3 ,nobody ,leadoff);      %long fly out
a(3 )=a(1);                                                   %hard ground out
a(4 )=a(2);                                                   %soft ground out
a(5 )=V(ondeck,d            ,outs,b12 ,batter,leadoff);       %walk
a(6 )=V(ondeck,min(dtop,d+1),outs,b1  ,batter,leadoff);       %short single
a(7 )=V(ondeck,min(dtop,d+1),outs,b1  ,batter,leadoff);       %long single
a(8 )=V(ondeck,min(dtop,d+1),outs,b2  ,nobody,leadoff);       %short double
a(9 )=V(ondeck,min(dtop,d+1),outs,b2  ,nobody,leadoff);       %long double
a(10)=V(ondeck,min(dtop,d+1),outs,b3  ,nobody,leadoff);       %triple
a(11)=V(ondeck,min(dtop,d+2),outs,b0  ,nobody,leadoff);       %home run
walk=a(5);
hitaway=probs(batter,:)*a;
V(batter,d,outs,b2,onfirst,leadoff)=min(hitaway,walk);
if V(batter,d,outs,b2,onfirst,leadoff)==walk
   strategy(batter,d,outs,b2,onfirst,leadoff)=4;
end

end
end
end

%                                       1 out, man on 1st

for batter=1:9
   if batter == 9
      ondeck=1;
   else
      ondeck=batter+1;
   end
for onfirst=1:9
for leadoff=1:9

outs=outs1;
a(1 )=V(ondeck,d            ,outs2,b1 ,onfirst,leadoff);      %strike out or short fly out
a(2 )=a(1);                                                   %long fly out
a(3 )=W(d,leadoff,ondeck);                                    %hard ground out
a(4 )=V(ondeck,d            ,outs2,b1 ,batter,leadoff);       %soft ground out
a(5 )=V(ondeck,d            ,outs,b12 ,batter,leadoff);       %walk
a(6 )=V(ondeck,d            ,outs,b12 ,batter,leadoff);       %short single
a(7 )=V(ondeck,d            ,outs,b13 ,batter,leadoff);       %long single
a(8 )=V(ondeck,d            ,outs,b23 ,nobody,leadoff);       %short double
a(9 )=V(ondeck,min(dtop,d+1),outs,b2  ,nobody,leadoff);       %long double
a(10)=V(ondeck,min(dtop,d+1),outs,b3  ,nobody,leadoff);       %triple
a(11)=V(ondeck,min(dtop,d+2),outs,b0  ,nobody,leadoff);       %home run
walk=a(5);
bunt=sacbunt(batter)*V(ondeck,d,outs2,b2,nobody,leadoff) ...
       +(1-sacbunt(batter))*V(ondeck,d,outs2,b1,batter,leadoff);
hitaway=probs(batter,:)*a;
steal=psteal(onfirst)*V(batter,d,outs,b2,nobody,leadoff) ...
                 +(1-psteal(onfirst))*V(batter,d,outs2,b0,nobody,leadoff);
V(batter,d,outs,b1,onfirst,leadoff)=min(max([hitaway,steal,bunt]),walk);
if V(batter,d,outs,b1,onfirst,leadoff)==walk
   strategy(batter,d,outs,b1,onfirst,leadoff)=4;
elseif V(batter,d,outs,b1,onfirst,leadoff)==steal 
   strategy(batter,d,outs,b1,onfirst,leadoff)=2; 
elseif V(batter,d,outs,b1,onfirst,leadoff)==bunt 
   strategy(batter,d,outs,b1,onfirst,leadoff)=3;   
end

end
end
end

%                                       1 out, bases empty

for batter=1:9
   if batter == 9
      ondeck=1;
   else
      ondeck=batter+1;
   end
for onfirst=1:9
for leadoff=1:9

outs=outs1;
a(1 )=V(ondeck,d            ,outs2,b0 ,nobody ,leadoff);      %strike out or short fly out
a(2 )=a(1);                                                   %long fly out
a(3 )=a(1);                                                   %hard ground out
a(4 )=a(1);                                                   %soft ground out
a(5 )=V(ondeck,d            ,outs,b1  ,batter,leadoff);       %walk
a(6 )=V(ondeck,d            ,outs,b1  ,batter,leadoff);       %short single
a(7 )=V(ondeck,d            ,outs,b1  ,batter,leadoff);       %long single
a(8 )=V(ondeck,d            ,outs,b2  ,nobody,leadoff);       %short double
a(9 )=V(ondeck,d            ,outs,b2  ,nobody,leadoff);       %long double
a(10)=V(ondeck,d            ,outs,b3  ,nobody,leadoff);       %triple
a(11)=V(ondeck,min(dtop,d+1),outs,b0  ,nobody,leadoff);       %home run
walk=a(5);
hitaway=probs(batter,:)*a;
V(batter,d,outs,b0,onfirst,leadoff)=min(hitaway,walk);
if V(batter,d,outs,b0,onfirst,leadoff)==walk
   strategy(batter,d,outs,b0,onfirst,leadoff)=4;
end

end
end
end

%                                       0 out, bases loaded

for batter=1:9
   if batter == 9
      ondeck=1;
   else
      ondeck=batter+1;
   end
for onfirst=1:9
for leadoff=1:9

outs=outs0;
a(1 )=V(ondeck,d            ,outs1,b123,onfirst,leadoff);     %strike out or short fly out
a(2 )=V(ondeck,min(dtop,d+1),outs1,b12 ,onfirst,leadoff);     %long fly out
a(3 )=min(V(ondeck,min(dtop,d+1),outs2,b3  ,nobody ,leadoff), ...
                 V(ondeck,d,outs1,b123  ,batter ,leadoff));   %hard ground out
a(4 )=V(ondeck,d            ,outs1,b123,batter,leadoff);      %soft ground out
a(5 )=V(ondeck,min(dtop,d+1),outs,b123 ,batter,leadoff);      %walk
a(6 )=V(ondeck,min(dtop,d+2),outs,b12  ,batter,leadoff);      %short single
a(7 )=V(ondeck,min(dtop,d+2),outs,b13  ,batter,leadoff);      %long single
a(8 )=V(ondeck,min(dtop,d+2),outs,b23  ,nobody,leadoff);      %short double
a(9 )=V(ondeck,min(dtop,d+3),outs,b2   ,nobody,leadoff);      %long double
a(10)=V(ondeck,min(dtop,d+3),outs,b3   ,nobody,leadoff);      %triple
a(11)=V(ondeck,min(dtop,d+4),outs,b0   ,nobody,leadoff);      %home run
walk=a(5);
hitaway=probs(batter,:)*a;
V(batter,d,outs,b123,onfirst,leadoff)=min(hitaway,walk);
if V(batter,d,outs,b123,onfirst,leadoff)==walk
   strategy(batter,d,outs,b123,onfirst,leadoff)=4;
end

end
end
end

%                                       0 out, men on 2nd and 3rd

for batter=1:9
   if batter == 9
      ondeck=1;
   else
      ondeck=batter+1;
   end
for onfirst=1:9
for leadoff=1:9

outs=outs0;
a(1 )=V(ondeck,d            ,outs1,b23,nobody ,leadoff);      %strike out or short fly out
a(2 )=V(ondeck,min(dtop,d+1),outs1,b3 ,nobody ,leadoff);      %long fly out
a(3 )=a(1);                                                   %hard ground out
a(4 )=V(ondeck,min(dtop,d+1),outs1,b3 ,nobody,leadoff);       %soft ground out
a(5 )=V(ondeck,d            ,outs,b123,batter,leadoff);       %walk
a(6 )=V(ondeck,min(dtop,d+2),outs,b1  ,batter,leadoff);       %short single
a(7 )=V(ondeck,min(dtop,d+2),outs,b1  ,batter,leadoff);       %long single
a(8 )=V(ondeck,min(dtop,d+2),outs,b2  ,nobody,leadoff);       %short double
a(9 )=V(ondeck,min(dtop,d+2),outs,b2  ,nobody,leadoff);       %long double
a(10)=V(ondeck,min(dtop,d+2),outs,b3  ,nobody,leadoff);       %triple
a(11)=V(ondeck,min(dtop,d+3),outs,b0  ,nobody,leadoff);       %home run
walk=a(5);
hitaway=probs(batter,:)*a;
V(batter,d,outs,b23,onfirst,leadoff)=min(hitaway,walk);
if V(batter,d,outs,b23,onfirst,leadoff)==walk
   strategy(batter,d,outs,b23,onfirst,leadoff)=4;
end

end
end
end

%                                       0 out, men on 1st and 3rd

for batter=1:9
   if batter == 9
      ondeck=1;
   else
      ondeck=batter+1;
   end
for onfirst=1:9
for leadoff=1:9

outs=outs0;
a(1 )=V(ondeck,d            ,outs1,b13,onfirst,leadoff);      %strike out or short fly out
a(2 )=V(ondeck,min(dtop,d+1),outs1,b1 ,onfirst,leadoff);      %long fly out
a(3 )=min(V(ondeck,min(dtop,d+1),outs2,b0 ,nobody,leadoff), ...
                 V(ondeck,d,outs1,b13 ,batter,leadoff));      %hard ground out
a(4 )=V(ondeck,min(dtop,d+1),outs1,b2 ,nobody,leadoff);       %soft ground out
a(5 )=V(ondeck,d            ,outs,b123,batter,leadoff);       %walk
a(6 )=V(ondeck,min(dtop,d+1),outs,b12 ,batter,leadoff);       %short single
a(7 )=V(ondeck,min(dtop,d+1),outs,b13 ,batter,leadoff);       %long single
a(8 )=V(ondeck,min(dtop,d+1),outs,b23 ,nobody,leadoff);       %short double
a(9 )=V(ondeck,min(dtop,d+2),outs,b2  ,nobody,leadoff);       %long double
a(10)=V(ondeck,min(dtop,d+2),outs,b3  ,nobody,leadoff);       %triple
a(11)=V(ondeck,min(dtop,d+3),outs,b0  ,nobody,leadoff);       %home run
walk=a(5);
hitaway=probs(batter,:)*a;
steal=psteal(onfirst)*V(batter,d,outs,b23,nobody,leadoff) ...
                 +(1-psteal(onfirst))*V(batter,d,outs1,b3,nobody,leadoff);
V(batter,d,outs,b13,onfirst,leadoff)=min(max(hitaway,steal),walk);
if V(batter,d,outs,b13,onfirst,leadoff)==walk
   strategy(batter,d,outs,b13,onfirst,leadoff)=4;
elseif V(batter,d,outs,b13,onfirst,leadoff)==steal 
   strategy(batter,d,outs,b13,onfirst,leadoff)=2; 
end

end
end
end

%                                       0 out, men on 1st and 2nd

for batter=1:9
   if batter == 9
      ondeck=1;
   else
      ondeck=batter+1;
   end
for onfirst=1:9
for leadoff=1:9

outs=outs0;
a(1 )=V(ondeck,d            ,outs1,b12,onfirst,leadoff);      %strike out or short fly out
a(2 )=V(ondeck,d            ,outs1,b13,onfirst,leadoff);      %long fly out
a(3 )=V(ondeck,d            ,outs2,b3, nobody,leadoff);       %hard ground out
a(4 )=V(ondeck,d            ,outs1,b12,batter,leadoff);       %soft ground out
a(5 )=V(ondeck,d            ,outs,b123,batter,leadoff);       %walk
a(6 )=V(ondeck,min(dtop,d+1),outs,b12 ,batter,leadoff);       %short single
a(7 )=V(ondeck,min(dtop,d+1),outs,b13 ,batter,leadoff);       %long single
a(8 )=V(ondeck,min(dtop,d+1),outs,b23 ,nobody,leadoff);       %short double
a(9 )=V(ondeck,min(dtop,d+2),outs,b2  ,nobody,leadoff);       %long double
a(10)=V(ondeck,min(dtop,d+2),outs,b3  ,nobody,leadoff);       %triple
a(11)=V(ondeck,min(dtop,d+3),outs,b0  ,nobody,leadoff);       %home run
walk=a(5);
bunt=sacbunt(batter)*V(ondeck,d,outs1,b23,nobody,leadoff) ...
       +(1-sacbunt(batter))*V(ondeck,d,outs1,b12,batter,leadoff);
hitaway=probs(batter,:)*a;
V(batter,d,outs,b12,onfirst,leadoff)=min(max(hitaway,bunt),walk);
if V(batter,d,outs,b12,onfirst,leadoff)==walk
   strategy(batter,d,outs,b12,onfirst,leadoff)=4;
elseif V(batter,d,outs,b12,onfirst,leadoff)==bunt 
   strategy(batter,d,outs,b12,onfirst,leadoff)=3; 
end

end
end
end

%                                       0 out, man on 3rd

for batter=1:9
   if batter == 9
      ondeck=1;
   else
      ondeck=batter+1;
   end
for onfirst=1:9
for leadoff=1:9

outs=outs0;
a(1 )=V(ondeck,d            ,outs1,b3 ,nobody ,leadoff);      %strike out or short fly out
a(2 )=V(ondeck,min(dtop,d+1),outs1,b0 ,nobody ,leadoff);      %long fly out
a(3 )=a(1);                                                   %hard ground out
a(4 )=a(2);                                                   %soft ground out
a(5 )=V(ondeck,d            ,outs,b13 ,batter,leadoff);       %walk
a(6 )=V(ondeck,min(dtop,d+1),outs,b1  ,batter,leadoff);       %short single
a(7 )=V(ondeck,min(dtop,d+1),outs,b1  ,batter,leadoff);       %long single
a(8 )=V(ondeck,min(dtop,d+1),outs,b2  ,nobody,leadoff);       %short double
a(9 )=V(ondeck,min(dtop,d+1),outs,b2  ,nobody,leadoff);       %long double
a(10)=V(ondeck,min(dtop,d+1),outs,b3  ,nobody,leadoff);       %triple
a(11)=V(ondeck,min(dtop,d+2),outs,b0  ,nobody,leadoff);       %home run
walk=a(5);
hitaway=probs(batter,:)*a;
V(batter,d,outs,b3,onfirst,leadoff)=min(hitaway,walk);
if V(batter,d,outs,b3,onfirst,leadoff)==walk
   strategy(batter,d,outs,b3,onfirst,leadoff)=4;
end

end
end
end

%                                       0 out, man on 2nd

for batter=1:9
   if batter == 9
      ondeck=1;
   else
      ondeck=batter+1;
   end
for onfirst=1:9
for leadoff=1:9

outs=outs0;
a(1 )=V(ondeck,d            ,outs1,b2 ,nobody ,leadoff);      %strike out or short fly out
a(2 )=V(ondeck,d            ,outs1,b3 ,nobody ,leadoff);      %long fly out
a(3 )=a(1);                                                   %hard ground out
a(4 )=a(2);                                                   %soft ground out
a(5 )=V(ondeck,d            ,outs,b12 ,batter,leadoff);       %walk
a(6 )=V(ondeck,min(dtop,d+1),outs,b1  ,batter,leadoff);       %short single
a(7 )=V(ondeck,min(dtop,d+1),outs,b1  ,batter,leadoff);       %long single
a(8 )=V(ondeck,min(dtop,d+1),outs,b2  ,nobody,leadoff);       %short double
a(9 )=V(ondeck,min(dtop,d+1),outs,b2  ,nobody,leadoff);       %long double
a(10)=V(ondeck,min(dtop,d+1),outs,b3  ,nobody,leadoff);       %triple
a(11)=V(ondeck,min(dtop,d+2),outs,b0  ,nobody,leadoff);       %home run
walk=a(5);
hitaway=probs(batter,:)*a;
V(batter,d,outs,b2,onfirst,leadoff)=min(hitaway,walk);
if V(batter,d,outs,b2,onfirst,leadoff)==walk
   strategy(batter,d,outs,b2,onfirst,leadoff)=4;
end

end
end
end

%                                       0 out, man on 1st

for batter=1:9
   if batter == 9
      ondeck=1;
   else
      ondeck=batter+1;
   end
for onfirst=1:9
for leadoff=1:9

outs=outs0;
a(1 )=V(ondeck,d            ,outs1,b1 ,onfirst,leadoff);      %strike out or short fly out
a(2 )=a(1);                                                   %long fly out
a(3 )=V(ondeck,d            ,outs2,b0 ,nobody ,leadoff);      %hard ground out
a(4 )=V(ondeck,d            ,outs1,b1 ,batter,leadoff);       %soft ground out
a(5 )=V(ondeck,d            ,outs,b12 ,batter,leadoff);       %walk
a(6 )=V(ondeck,d            ,outs,b12 ,batter,leadoff);       %short single
a(7 )=V(ondeck,d            ,outs,b13 ,batter,leadoff);       %long single
a(8 )=V(ondeck,d            ,outs,b23 ,nobody,leadoff);       %short double
a(9 )=V(ondeck,min(dtop,d+1),outs,b2  ,nobody,leadoff);       %long double
a(10)=V(ondeck,min(dtop,d+1),outs,b3  ,nobody,leadoff);       %triple
a(11)=V(ondeck,min(dtop,d+2),outs,b0  ,nobody,leadoff);       %home run
walk=a(5);
bunt=sacbunt(batter)*V(ondeck,d,outs1,b2,nobody,leadoff) ...
       +(1-sacbunt(batter))*V(ondeck,d,outs1,b1,batter,leadoff);
hitaway=probs(batter,:)*a;
steal=psteal(onfirst)*V(batter,d,outs,b2,nobody,leadoff) ...
                 +(1-psteal(onfirst))*V(batter,d,outs1,b0,nobody,leadoff);
V(batter,d,outs,b1,onfirst,leadoff)=min(max([hitaway,steal,bunt]),walk);
if V(batter,d,outs,b1,onfirst,leadoff)==walk
   strategy(batter,d,outs,b1,onfirst,leadoff)=4;
elseif V(batter,d,outs,b1,onfirst,leadoff)==steal 
   strategy(batter,d,outs,b1,onfirst,leadoff)=2; 
elseif V(batter,d,outs,b1,onfirst,leadoff)==bunt 
   strategy(batter,d,outs,b1,onfirst,leadoff)=3;   
end

end
end
end

%                                       0 out, bases empty

for batter=1:9
   if batter == 9
      ondeck=1;
   else
      ondeck=batter+1;
   end
for onfirst=1:9
for leadoff=1:9

outs=outs0;
a(1 )=V(ondeck,d            ,outs1,b0 ,nobody ,leadoff);      %strike out or short fly out
a(2 )=a(1);                                                   %long fly out
a(3 )=a(1);                                                   %hard ground out
a(4 )=a(1);                                                   %soft ground out
a(5 )=V(ondeck,d            ,outs,b1  ,batter,leadoff);       %walk
a(6 )=V(ondeck,d            ,outs,b1  ,batter,leadoff);       %short single
a(7 )=V(ondeck,d            ,outs,b1  ,batter,leadoff);       %long single
a(8 )=V(ondeck,d            ,outs,b2  ,nobody,leadoff);       %short double
a(9 )=V(ondeck,d            ,outs,b2  ,nobody,leadoff);       %long double
a(10)=V(ondeck,d            ,outs,b3  ,nobody,leadoff);       %triple
a(11)=V(ondeck,min(dtop,d+1),outs,b0  ,nobody,leadoff);       %home run
walk=a(5);
hitaway=probs(batter,:)*a;
V(batter,d,outs,b0,onfirst,leadoff)=min(hitaway,walk);
if V(batter,d,outs,b0,onfirst,leadoff)==walk
   strategy(batter,d,outs,b0,onfirst,leadoff)=4;
end

end
end
end

end