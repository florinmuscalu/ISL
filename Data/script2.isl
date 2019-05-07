DEFINE
 num X=9;
END DEFINE

func OnCreate:bool
	skyboxclear();
	PSAdd(0, X ,1, 10); #adauga sistemul de particule cu id-ul 0 la pozitia (10,1,10)
	PSAdd(1, X ,1, 10); #adauga sistemul de particule cu id-ul 1 la pozitia (9, 1,10)	
	PSAdd(2, X ,1, 10); #adauga sistemul de particule cu id-ul 2 la pozitia (9, 1,10)
	PSAdd(3, X ,1, 10);	#adauga sistemul de particule cu id-ul 3 la pozitia (9, 1,10)	
	
	PSLoad(0, "fire.ps");	 #incarca setarile pentru sistemul de particule 0
	PSLoad(1, "fire1.ps");	 #incarca setarile pentru sistemul de particule 1
	PSLoad(2, "fire1.ps");	 #incarca setarile pentru sistemul de particule 2
	PSLoad(3, "fire2.ps");	 #incarca setarile pentru sistemul de particule 3
	result:=true;
end func

func OnIdle(num DeltaTime):bool
	num delta;
	delta:=(DeltaTime*pi/180)*7;		                                                   #transforma din grade in radiani
	PSSetPos(0, X+0.7*sin(delta)*cos(delta), 1+0.8*cos(delta), 10+0.9*cos(delta));#actualizeaza pozitia sistemului de particule 0
	PSSetPos(1, X+0.8*cos(delta), 1+0.7*sin(delta), 10+0.9*sin(delta));	   #actualizeaza pozitia sistemului de particule 1
	PSSetPos(2, X+0.9*sin(delta), 1+0.8*cos(sin(delta)), 10);			   #actualizeaza pozitia sistemului de particule 2
	
	result:=true;
end func
