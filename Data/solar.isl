DEFINE
 num X=9;
END DEFINE

func OnCreate:bool
	skyboxload("skybox5");
	PSAdd(0, X ,1, 10); #Soarele
	PSAdd(1, X ,1, 10); #Mercur
	PSAdd(2, X ,1, 10);	#Venus
	PSAdd(3, X ,1, 10);	#Pamant
	PSAdd(4, X ,1, 10);	#Marte
	PSLoad(0, "fire2.ps");	 #incarca setarile Soare
	PSLoad(1, "fire1.ps");	 #Mercur
	PSLoad(2, "fire1.ps");	 #Venus
	PSLoad(3, "fire1.ps");	 #Pamant
	PSLoad(4, "fire1.ps");	 #Marte
	result:=true;
end func

func OnIdle(num DeltaTime):bool
	num delta, d, p;
	
	delta:=(DeltaTime*pi/180)*0.5;		#transforma din grade in radiani
	d:=2*57.9/149.6; p:=1*365/88; PSSetPos(1, X+d*cos(delta*p), 1+d*sin(delta*p), 10); #Mercur
	d:=2*108.2/149.6; p:=1*365/165; PSSetPos(2, X+d*cos(delta*p), 1+d*sin(delta*p), 10); #Venus
	d:=2; p:=1; PSSetPos(3, X+d*cos(delta*p), 1+d*sin(delta*p), 10); #Pamant
	d:=2*227.9/149.6; p:=1*365/687; PSSetPos(4, X+d*cos(delta*p), 1+d*sin(delta*p), 10); #Marte
	result:=true;
end func
