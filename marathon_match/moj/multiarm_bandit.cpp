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
	void setseed(uint32_t seed){y=seed;}
	uint32_t nextInt() {
    	return y^=(y^=(y^=y<<13)>>17)<<5;
	}
	uint32_t nextInt(uint32_t n) {
		return nextInt()%n;
	}
	double nextDouble() {return nextInt()*iDouble;}
} rnd;



double P[51];
int playslot(int i){
    //if(rnd.nextDouble()<=P[i]) return 1;
    //else return 0;
    
    cout<<i<<endl;
    int res;
    cin>>res;
    return res;
    
}
class ucb1{
public:
	int n;
	int totalcount=0;
	double weight=1.0;
	vector<double> ave;
	vector<int> armcount;
	vector<double> val;
	ucb1(){}
	void init(int _n,double _w=1.0){
		n=_n;totalcount=0;weight=_w;
		ave.resize(_n);
		armcount.resize(_n);
		val.resize(_n);
	}
	int initplay(int bet){
		int sum=0,ret;
		rep(i,n){
			++totalcount;
			++armcount[i];
			ret=playslot(i);
			ave[i]=ret;
			sum+=ret-bet;
		}
		return sum;
	}
	pint select_arm(int bet){
		int id,ret;
		rep(i,n){
			val[i]=ave[i]*weight+sqrt(2.0*log(totalcount)/armcount[i]);
		}
		id=max_element(val.begin(),val.end())-val.begin();
		++totalcount;++armcount[id];
		ret=playslot(id);
		ave[id]+=((double)ret-ave[id])/armcount[id];
		return {id,ret-bet};
	}
};
class ucb1_tuned{
public:
	int n;
	int totalcount=0;
	double weight=1.0;
	vector<double> ave,ave2;
	vector<int> armcount;
	vector<double> val;
	ucb1_tuned(){}
	void init(int _n,double _w=1.0){
		n=_n;totalcount=0;weight=_w;
		ave.resize(_n);ave2.resize(_n);
		armcount.resize(_n);
		val.resize(_n);
	}
	int initplay(int bet){
		int sum=0,ret;
		rep(i,n){
			++totalcount;
			++armcount[i];
			ret=playslot(i);
			ave[i]=ret;ave2[i]=ret*ret;
			sum+=ret-bet;
		}
		return sum;
	}
	pint select_arm(int bet){
		int id,ret;
		rep(i,n){
			val[i]=
			ave[i]*weight+sqrt(log(totalcount)/armcount[i]
			*min(0.25,ave2[i]-ave[i]*ave[i]+sqrt(2.0*log(totalcount)/armcount[i])));
		}
		id=max_element(val.begin(),val.end())-val.begin();
		++totalcount;++armcount[id];
		ret=playslot(id);
		ave[id]+=((double)ret-ave[id])/armcount[id];
		ave2[id]+=((double)ret*ret-ave2[id])/armcount[id];
		return {id,ret-bet};
	}
};
class ucb2{
public:
	int n;
	double e,alpha=0;//a:(0,1) 0.0001-0.1
	int totalcount=0,nextcount=0,curarm=0;
	double weight=1.0;
	vector<double> ave;
	vector<int> armcount;
	vector<double> val;
	ucb2(){}
	void init(int _n,double _a,double _w=1.0){
		n=_n;totalcount=0;nextcount=0;alpha=_a;weight=_w;
		e=exp(1.0);
		ave.resize(_n);
		armcount.resize(_n);
		val.resize(_n);
	}
	int tau(int r){
		return (int)ceil(pow(alpha+1.0,r));
	}
	int initplay(int bet){
		int sum=0,ret;
		rep(i,n){
			++totalcount;
			++armcount[i];
			ret=playslot(i);
			ave[i]=ret;
			sum+=ret-bet;
		}
		curarm=0;
		nextcount=totalcount+max(1,tau(2)-tau(1));
		return sum;
	}
	pint select_arm(int bet){
		int id,ret;
		if(nextcount>totalcount){
			id=curarm;
		}
		else{
			rep(i,n){
				val[i]=ave[i]*weight+sqrt(0.5*(alpha+1.0)*log(e*totalcount/tau(armcount[i]))/tau(armcount[i]));
			}
			id=max_element(val.begin(),val.end())-val.begin();
			curarm=id;
			nextcount+=max(1,tau(armcount[id]+1)-tau(armcount[id]));
			++armcount[id];
		}
		++totalcount;
		ret=playslot(id);
		ave[id]+=((double)ret-ave[id])/armcount[id];
		return {id,ret-bet};
	}
};


int N,T;
int main() {
    cin>>N>>T;
    /*
    rep(i,N) P[i]=0.2+0.6*i/(N-1);
    for(int i=N-1;i>0;--i){
        int j=rnd.nextInt(i+1);
        swap(P[i],P[j]);
    }
    */
    int score=0;
    ucb1 ucb;
    ucb.init(N,15);//tekito-
    score=ucb.initplay(0);
    FOR(_,N,T){
        score+=ucb.select_arm(0).second;
    }
    cerr<<score<<endl;
	return 0;
}