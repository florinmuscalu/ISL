DEFINE
 num X=9;
END DEFINE

func OnCreate:bool
	skyboxload("skybox5");
	PSAdd(0, X ,1, 10); #adauga sistemul de particule cu id-ul 0 la pozitia (10,1,10)
	PSAdd(1, X ,1, 10); #adauga sistemul de particule cu id-ul 1 la pozitia (9, 1,10)	
	PSAdd(2, X ,1, 10);	#adauga sistemul de particule cu id-ul 2 la pozitia (9, 1,10)
	PSLoad(0, "Nicefire.ps");	 #incarca setarile pentru sistemul de particule 0
	PSLoad(1, "fire2.ps");	 #incarca setarile pentru sistemul de particule 1
	PSLoad(2, "fire1.ps");	 #incarca setarile pentru sistemul de particule 2
	result:=true;
end func

func OnIdle(num DeltaTime):bool
	num delta;
	delta:=(DeltaTime*pi/180)*5;		#transforma din grade in radiani
	PSSetPos(0, X+sin(delta/5)*3, 1+cos(delta/2), 10 );		#actualizeaza pozitia sistemului de particule 0
	PSSetPos(1, X+cos(delta), 1+sin(delta)             , 10);		#actualizeaza pozitia sistemului de particule 1
	
	if cos(delta)>0
		PSSetPos(2, X+sin(delta), 1+cos(delta), 10);		#actualizeaza pozitia sistemului de particule 2
	else
		PSSetPos(2, X-2*sin(delta/3), 1-2*cos(delta/3), 10);	#actualizeaza pozitia sistemului de particule 2
	end;
	result:=true;
end func
