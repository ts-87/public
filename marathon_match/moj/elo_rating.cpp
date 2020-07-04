#include <bits/stdc++.h>
using namespace std;
#define FOR(i,a,b) for(int i=(a);i<(b);++i)
#define rep(i,n)   FOR(i,0,n)
#define pb emplace_back
typedef long long ll;
typedef pair<int,int> pint;
typedef pair<double,int> pdi;

struct Rand2 {
	uint32_t y;
	static constexpr double iDouble=1.0/(1LL<<32);
	Rand2() {
		y=2463534242;
	}
	void setseed(uint32_t seed){y^=seed;}
	uint32_t nextInt() {
    	return y^=(y^=(y^=y<<13)>>17)<<5;
	}
	uint32_t nextInt(uint32_t n) {
		return nextInt()%n;
	}
	double nextDouble() {return nextInt()*iDouble;}
} rnd;

double actualR[101],predictR[101];
double K=16;
double initK=20,endK=10;
int N,T,turn;
void makecase(int seed=0){
	rnd.setseed(seed);
	rep(i,20) rnd.nextInt();
	rep(i,N) actualR[i]=rnd.nextDouble()*1000;
}
inline void update(int a,int b,bool awin){
	double tmp,tKa,tKb;
	
	tKa=max(endK,initK+(endK-initK)/T*turn);
	tKb=max(endK,initK+(endK-initK)/T*turn);
	if(awin){
		tmp=1.0/(pow(10,(predictR[a]-predictR[b])/400)+1);
		predictR[a]+=tKa*tmp;
		predictR[b]-=tKb*tmp;
	}
	else{
		tmp=1.0/(pow(10,(predictR[b]-predictR[a])/400)+1);
		predictR[a]-=tKa*tmp;
		predictR[b]+=tKb*tmp;
	}
	
	/*
	if(awin){
		tmp=1.0/(pow(10,(predictR[a]-predictR[b])/400)+1);
		predictR[a]+=K*tmp;
		predictR[b]-=K*tmp;
	}
	else{
		tmp=1.0/(pow(10,(predictR[b]-predictR[a])/400)+1);
		predictR[a]-=K*tmp;
		predictR[b]+=K*tmp;
	}
	*/
}
int match(){
	int a,b;
	/*
	while(1){
		a=rnd.nextInt(N),b=rnd.nextInt(N);
		if(a!=b) break;
	}
	*/
	cin>>a>>b;

	char y,x=(predictR[a]>=predictR[b]?'>':'<');
	cout<<x<<endl;
	/*
	double wa=1.0/(pow(10,(actualR[b]-actualR[a])/400)+1);
	if(rnd.nextDouble()<wa) y='>';
	else y='<';
	*/
	cin>>y;
	update(a,b,y=='>');
	return (int)(x==y);
}
int main() {
    cin>>N>>T;
	//N=100,T=10000;
	rep(i,N) predictR[i]=500;
	//makecase(14246890);
	int score=0;
	for(turn=0;turn<T;++turn){
		score+=match();
	}
    cerr<<score<<endl;
	return 0;
}