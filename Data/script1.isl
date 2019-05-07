DEFINE
 num X=9;
END DEFINE
func OnCreate:bool
	skyboxload("skybox2");
	PSAdd(0, 10,1, 10);			#adauga sistemul de particule cu id-ul 0 la pozitia (10,1,10)
	PSAdd(1, X ,1, 10);			#adauga sistemul de particule cu id-ul 1 la pozitia (9, 1,10)
	PSAdd(2, X ,1, 10);			#adauga sistemul de particule cu id-ul 2 la pozitia (9, 1,10)
	PSAdd(3, X ,1, 10);			#adauga sistemul de particule cu id-ul 2 la pozitia (9, 1,10)

	PSLoad(0, "nicefire.ps");		#incarca setarile pentru sistemul de particule 0
	PSLoad(1, "nicefire.ps");		#incarca setarile pentru sistemul de particule 1
	PSLoad(2, "nicefire.ps");		#incarca setarile pentru sistemul de particule 1
	PSLoad(3, "fire2.ps");			#incarca setarile pentru sistemul de particule 2
	result:=true;
end func

func OnDestroy:bool
	result:=true;
end func

func OnIdle(num DeltaTime):bool
	num delta;
	delta:=(DeltaTime*pi/180)*10;				#transforma din grade in radiani
	PSSetPos(0, X+sin(delta), 1+cos(delta/10)/2, 10+cos(delta));		#actualizeaza pozitia sistemului de particule 0
	PSSetPos(1, X+sin(delta), 1+sin(delta/10)/2, 10+cos(delta));		#actualizeaza pozitia sistemului de particule 1
	PSSetPos(2, X+cos(delta), 1+sin(delta/10)  , 10+sin(delta));		#actualizeaza pozitia sistemului de particule 2
	PSSetPos(3, X+cos(delta/10),            1+cos(delta/10), 10+cos(delta/10));	#actualizeaza pozitia sistemului de particule 3
	result:=true;
end func
