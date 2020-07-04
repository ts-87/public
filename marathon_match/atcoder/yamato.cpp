#include <bits/stdc++.h>
using namespace std;
#define FOR(i,a,b) for(int i=(a);i<(b);++i)
#define rep(i,n)   FOR(i,0,n)
#define pb emplace_back
typedef long long ll;
typedef pair<int,int> pint;
typedef pair<double,double> pdd;

const double LIMS=14400,SP1=250,SP2=100;
const int DIVB=4;
double LN[65536];

int N,M;
string PID[40],DID[150];
int pidnum[40],revP[40];
//vector<int> GP[40];
double Pcost[40][2],Dcost[150];
int Dlim[150];
double PPcost[41][41],DDcost[190][190],DPcost[150][40];
bool ufP[41],dup[200];
double totcost,avedist[190],finalscore;

vector<int> tg1;
unordered_set<int> tg2,tg3;
vector<pint> ttg3;
int numt1,numt2,numc1,cntp[40];
vector<pint> candP,prP;
bool usedP[40],usedD[190];
vector<int> candD[40];
//int permP[100],bpermP[100],bpermP120[100];
vector<int> permP,bpermP,bpermP120,fpermP,ffpermP,cpermP,npermP;
vector<int> permD,preD,permD1,permD2;//bpermD[100],fpermD[100];
vector<vector<int> > bpermD,fpermD,ffpermD,cpermD,npermD;
int NP,ND,NP120=1,bNP,fNP,cNP,nNP;
vector<int> delay;
int intime120=1,intime240=1;
vector<int> neiP[40];

double timeLimit=58;
const int64_t CYCLES_PER_SEC=2800000000;
struct Timer {
	int64_t start;
	Timer() { reset(); }
	void reset() { start = getCycle(); }
	inline double get() { return (double)(getCycle() - start) / CYCLES_PER_SEC; }
	inline int64_t getCycle() {
		uint32_t low, high;
		__asm__ volatile ("rdtsc" : "=a" (low), "=d" (high));
		return ((int64_t)low) | ((int64_t)high << 32);
	}
};
Timer timer;
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

void inpt(){
    cin>>N>>M;
    memset(revP,-1,sizeof(revP));
    rep(i,N){
        cin>>PID[i]>>Pcost[i][0]>>Pcost[i][1];

        pidnum[i]=100*(PID[i][1]-'0')+10*(PID[i][2]-'0')+(PID[i][3]-'0');
    }
    rep(i,N)FOR(j,i+1,N){
        if(pidnum[i]==pidnum[j]) revP[i]=j,revP[j]=i;
    }
    rep(i,M){
        cin>>DID[i]>>Dcost[i]>>Dlim[i];

        if(Dlim[i]==120||Dlim[i]==240) tg1.pb(i);
        else Dlim[i]=1000,tg2.insert(i);
    }
    numt1=tg1.size();

    double d;
    int cnt=0;
    rep(i,N){
        cnt=0;
        rep(j,N){
            cin>>d;
            PPcost[i][j]=d/SP1;
            if(PPcost[i][j]<0) PPcost[i][j]=100000,++cnt;
        }
        if(cnt<=N/5) ufP[i]=true;
    }
    rep(i,M){
        rep(j,N){
            cin>>d;
            DPcost[i][j]=d/SP2;
            DDcost[i][j+M]=d/SP2;
            DDcost[j+M][i]=d/SP2;
        }
        double ave=0;
        rep(j,M){
            cin>>d;
            DDcost[i][j]=d/SP2;
            ave+=d/SP1;
        }
        avedist[i]=ave;
    }
}

inline pdd calcScore(){
    double cost=0,dcnt=0;
    int k;
    bool ok=true;
    rep(i,NP){
        k=bpermP[i];
        if(i>0){
            if(bpermP[i]!=bpermP[i-1])cost+=Pcost[k][1];
            cost+=PPcost[bpermP[i-1]][k];
        }
        rep(j,bpermD[i].size()){
            if(j==0) cost+=DPcost[bpermD[i][0]][k];
            else{
                cost+=DDcost[bpermD[i][j-1]][bpermD[i][j]];
            }
            if(bpermD[i][j]<M){
                cost+=Dcost[bpermD[i][j]];
                if(cost<=240)++dcnt;
                if(cost>Dlim[bpermD[i][j]]) ok=false;
            }
        }
        if(i<NP-1){
            cost+=DPcost[bpermD[i][bpermD[i].size()-1]][k];
            if(bpermP[i]!=bpermP[i+1])cost+=Pcost[k][0];
        }
    }
    if(ok) return {cost,dcnt*10000+max(0,(int)(14400-cost*60))};
    else return {cost,-1};
}

inline pdd calcScore2(){
    delay.clear();
    double cost=0,dcnt=0,ret=0;;
    int k;
    bool ok=true;
    rep(i,NP){
        k=fpermP[i];
        if(i>0){
            if(fpermP[i]!=fpermP[i-1])cost+=Pcost[k][1];
            cost+=PPcost[fpermP[i-1]][k];
        }
        rep(j,fpermD[i].size()){
            if(j==0) cost+=DPcost[fpermD[i][0]][k];
            else{
                cost+=DDcost[fpermD[i][j-1]][fpermD[i][j]];
            }
            if(fpermD[i][j]<M){
                cost+=Dcost[fpermD[i][j]];
                if(cost<=240)++dcnt,ret=cost;
                if(cost>Dlim[fpermD[i][j]]) delay.pb(i*M+j),ok=false;
            }
        }
        if(i<NP-1){
            cost+=DPcost[fpermD[i][fpermD[i].size()-1]][k];
            if(fpermP[i]!=fpermP[i+1])cost+=Pcost[k][0];
        }
        if(cost<120) intime120=i+1;
        if(cost<240) intime240=i+1;
    }
    //if(ok) return {cost,dcnt*10000+max(0,(int)(14400-cost*60))};
    if(ok) return {cost,dcnt*10000+max(0,(int)(14400-ret*60))};
    else return {cost,-1};
}

double solve_Ptsp(){
    
    if(NP==1||NP120>1&&NP-NP120<=1) return 0;
    double curscore=0,bestscore=0;
    FOR(i,NP120,NP){
        curscore+=PPcost[permP[i-1]][permP[i]];
    }
    
    bestscore=curscore;
    //cerr<<bestscore<<endl;

    double starttemp=2,endtemp=0.001,ts;

    int lp=max(5000,min((1<<NP),1<<20));
    int id1,id2,sel;
    double pre,nex;
    rep(i,lp){
        
        ts=starttemp+(endtemp-starttemp)*i/lp;

        sel=rnd.nextInt()&127;

        if(sel){
        id1=rnd.nextInt(NP-NP120)+NP120,id2=rnd.nextInt(NP-NP120)+NP120;
        if(id1==id2) ++id2;
        if(id2==NP) id2=NP120;
        if(id1>id2) swap(id1,id2);

        if(id2-id1==1){
            if(id2!=NP-1){
                pre=PPcost[permP[id1-1]][permP[id1]]+PPcost[permP[id1]][permP[id2]]
                    +PPcost[permP[id2]][permP[id2+1]];
                nex=PPcost[permP[id1-1]][permP[id2]]+PPcost[permP[id2]][permP[id1]]
                    +PPcost[permP[id1]][permP[id2+1]];
            }
            else{
                pre=PPcost[permP[id1-1]][permP[id1]]+PPcost[permP[id1]][permP[id2]];
                nex=PPcost[permP[id1-1]][permP[id2]]+PPcost[permP[id2]][permP[id1]];
            }
        }
        else{
            if(id2!=NP-1){
                pre=PPcost[permP[id1-1]][permP[id1]]+PPcost[permP[id1]][permP[id1+1]]
                    +PPcost[permP[id2-1]][permP[id2]]+PPcost[permP[id2]][permP[id2+1]];
                nex=PPcost[permP[id1-1]][permP[id2]]+PPcost[permP[id2]][permP[id1+1]]
                    +PPcost[permP[id2-1]][permP[id1]]+PPcost[permP[id1]][permP[id2+1]];
            }
            else{
                pre=PPcost[permP[id1-1]][permP[id1]]+PPcost[permP[id1]][permP[id1+1]]
                    +PPcost[permP[id2-1]][permP[id2]];
                nex=PPcost[permP[id1-1]][permP[id2]]+PPcost[permP[id2]][permP[id1+1]]
                    +PPcost[permP[id2-1]][permP[id1]];
            }
        }
        double diff=pre-nex;

        if(diff>=0||diff>LN[rnd.nextInt()&65535]*ts){
            swap(permP[id1],permP[id2]);
            curscore-=diff;
            if(curscore<bestscore){
                bestscore=curscore;
                bpermP=permP;
            }
        }
        }
        else{
            id1=-1;
            int dai=0;
            if(NP120>1) dai=NP120;
            rep(i,10){
                id1=rnd.nextInt(NP-dai)+dai;
                if(revP[permP[id1]]!=-1) break;
            }
            if(revP[permP[id1]]==-1) continue;
            else id2=revP[permP[id1]];

            if(id1==NP-1){
                pre=PPcost[permP[id1-1]][permP[id1]];
                nex=PPcost[permP[id1-1]][id2];
            }
            else if(id1==0){
                pre=PPcost[permP[id1]][permP[id1+1]];
                nex=PPcost[id2][permP[id1+1]];
            }
            else{
                pre=PPcost[permP[id1-1]][permP[id1]]
                    +PPcost[permP[id1]][permP[id1+1]];
                nex=PPcost[permP[id1-1]][id2]
                    +PPcost[id2][permP[id1+1]];
                
            }

            double diff=pre-nex;

            if(diff>=0||diff>LN[rnd.nextInt()&65535]*ts){
                permP[id1]=id2;
                curscore-=diff;
                if(curscore<bestscore){
                    bestscore=curscore;
                    bpermP=permP;
                }
            }
        }
    }
    //cerr<<bestscore<<endl;
    return bestscore;
}

void solve_Dtsp(int ti,bool flag=false){
    int nd=permD.size();
    if(nd<=2) return;
    //permD.resize(nd);
    //rep(i,nd) permD[i]=bpermD[ti][i];
    double curscore=0,bestscore=0;

    curscore+=DPcost[permD[0]][permP[ti]];
    FOR(i,1,nd){
        curscore+=DDcost[permD[i-1]][permD[i]];
    }
    curscore+=DPcost[permD[nd-1]][permP[ti]];

    bestscore=curscore;

    //cerr<<bestscore<<endl;

    double starttemp=0.5,endtemp=0.005,ts;
    int mxiter=1<<20;
    if(flag) mxiter=50;
    int lp=max(100,min((1<<(nd)),mxiter));
    int id1,id2,sel;
    double pre,nex;
    rep(i,lp){

        ts=starttemp+(endtemp-starttemp)*i/lp;

        id1=rnd.nextInt(nd),id2=rnd.nextInt(nd);
        if(id1==id2) ++id2;
        if(id2==nd) id2=0;
        if(id1>id2) swap(id1,id2);

        if(id2==nd-1&&id1==0){
            pre=DPcost[permD[id1]][permP[ti]]+DPcost[permD[id2]][permP[ti]];
            nex=DPcost[permD[id2]][permP[ti]]+DPcost[permD[id1]][permP[ti]];
        }
        else if(id2==nd-1){
            pre=DDcost[permD[id1-1]][permD[id1]]+DPcost[permD[id2]][permP[ti]];
            nex=DDcost[permD[id1-1]][permD[id2]]+DPcost[permD[id1]][permP[ti]];
        }
        else if(id1==0){
            pre=DPcost[permD[id1]][permP[ti]]+DDcost[permD[id2]][permD[id2+1]];
            nex=DPcost[permD[id2]][permP[ti]]+DDcost[permD[id1]][permD[id2+1]];
        }
        else{
            pre=DDcost[permD[id1-1]][permD[id1]]+DDcost[permD[id2]][permD[id2+1]];
            nex=DDcost[permD[id1-1]][permD[id2]]+DDcost[permD[id1]][permD[id2+1]];
        }
        
        double diff=pre-nex;

        if(diff>=0||diff>LN[rnd.nextInt()&65535]*ts){
            reverse(permD.begin()+id1,permD.begin()+(id2+1));
            curscore-=diff;
            if(curscore<bestscore){
                bestscore=curscore;
                if(flag) fpermD[ti]=permD;
                else bpermD[ti]=permD;
            }
        }
    }
    /*
    curscore=0;
    curscore+=DPcost[bpermD[ti][0]][bpermP[ti]];
    FOR(i,1,nd){
        curscore+=DDcost[bpermD[ti][i-1]][bpermD[ti][i]];
    }
    curscore+=DPcost[bpermD[ti][nd-1]][permP[ti]];
    cerr<<curscore<<endl;
    cerr<<bestscore<<endl;
    */
}

//2-opt(reverse)追加するkamo
/*
void solve_Dvrp(int ti,bool flag=false){
    int nd=permD.size();

    double curscore=0,bestscore=0;

    curscore+=DPcost[permD[0]][permP[ti]];
    FOR(i,1,nd){
        curscore+=DDcost[permD[i-1]][permD[i]];
    }
    curscore+=DPcost[permD[nd-1]][permP[ti]];

    bestscore=curscore;

    //cerr<<bestscore<<endl;

    double starttemp=2,endtemp=0.001,ts;
    int mxiter=1<<20;
    if(flag) mxiter=5000;
    int lp=max(100,min((1<<(nd)),mxiter));
    int id1,id2,sel;
    double pre,nex;
    rep(i,lp){

        ts=starttemp+(endtemp-starttemp)*i/lp;

        while(1){
            id1=rnd.nextInt(nd),id2=rnd.nextInt(nd);
            if(permD[id1]<M&&permD[id2]<M&&id1!=id2) break;
        }
        if(id1>id2) swap(id1,id2);

        if(id2-id1==1){
            if(id2==nd-1){
                pre=DDcost[permD[id1-1]][permD[id1]]+DPcost[permD[id2]][bpermP[ti]];
                nex=DDcost[permD[id1-1]][permD[id2]]+DPcost[permD[id1]][bpermP[ti]];
            }
            else if(id1==0){
                pre=DPcost[permD[id1]][bpermP[ti]]+DDcost[permD[id2]][permD[id2+1]];
                nex=DPcost[permD[id2]][bpermP[ti]]+DDcost[permD[id1]][permD[id2+1]];
            }
            else{
                pre=DDcost[permD[id1-1]][permD[id1]]+DDcost[permD[id2]][permD[id2+1]];
                nex=DDcost[permD[id1-1]][permD[id2]]+DDcost[permD[id1]][permD[id2+1]];
            }
        }
        else{
            if(id1==0&&id2==nd-1){
                pre=DPcost[permD[id1]][bpermP[ti]]+DDcost[permD[id1]][permD[id1+1]]
                    +DDcost[permD[id2-1]][permD[id2]]+DPcost[permD[id2]][bpermP[ti]];
                nex=DPcost[permD[id2]][bpermP[ti]]+DDcost[permD[id2]][permD[id1+1]]
                    +DDcost[permD[id2-1]][permD[id1]]+DPcost[permD[id1]][bpermP[ti]];
            }
            else if(id2==nd-1){
                pre=DDcost[permD[id1-1]][permD[id1]]+DDcost[permD[id1]][permD[id1+1]]
                    +DDcost[permD[id2-1]][permD[id2]]+DPcost[permD[id2]][bpermP[ti]];
                nex=DDcost[permD[id1-1]][permD[id2]]+DDcost[permD[id2]][permD[id1+1]]
                    +DDcost[permD[id2-1]][permD[id1]]+DPcost[permD[id1]][bpermP[ti]];
            }
            else if(id1==0){
                pre=DPcost[permD[id1]][bpermP[ti]]+DDcost[permD[id1]][permD[id1+1]]
                    +DDcost[permD[id2-1]][permD[id2]]+DDcost[permD[id2]][permD[id2+1]];
                nex=DPcost[permD[id2]][bpermP[ti]]+DDcost[permD[id2]][permD[id1+1]]
                    +DDcost[permD[id2-1]][permD[id1]]+DDcost[permD[id1]][permD[id2+1]];
            }
            else{
                pre=DDcost[permD[id1-1]][permD[id1]]+DDcost[permD[id1]][permD[id1+1]]
                    +DDcost[permD[id2-1]][permD[id2]]+DDcost[permD[id2]][permD[id2+1]];
                nex=DDcost[permD[id1-1]][permD[id2]]+DDcost[permD[id2]][permD[id1+1]]
                    +DDcost[permD[id2-1]][permD[id1]]+DDcost[permD[id1]][permD[id2+1]];
            }
        }

        double diff=pre-nex;

        if(diff>=0||diff>LN[rnd.nextInt()&65535]*ts){
            swap(permD[id1],permD[id2]);
            curscore-=diff;
            if(curscore<bestscore){
                bestscore=curscore;
                //bpermD[ti]=permD;
                if(flag) fpermD[ti]=permD;
                else bpermD[ti]=permD;
            }
        }

    }

}
*/

void solve_Dvrp(int ti,bool flag=false){
    int nd=permD.size();

    double curscore=0,bestscore=0;

    curscore+=DPcost[permD[0]][permP[ti]];
    FOR(i,1,nd){
        curscore+=DDcost[permD[i-1]][permD[i]];
    }
    curscore+=DPcost[permD[nd-1]][permP[ti]];

    bestscore=curscore;

    //cerr<<bestscore<<endl;

    double starttemp=2,endtemp=0.001,ts;
    int mxiter=1<<20;
    if(flag) mxiter=300;
    int lp=max(100,min((1<<(nd)),mxiter));
    int id1,id2,sel;
    double pre,nex;
    rep(i,lp){

        ts=starttemp+(endtemp-starttemp)*i/lp;
        sel=rnd.nextInt()&7;
        if(sel){
        while(1){
            id1=rnd.nextInt(nd),id2=rnd.nextInt(nd);
            if(permD[id1]<M&&permD[id2]<M&&id1!=id2) break;
        }
        if(id1>id2) swap(id1,id2);

        if(id2-id1==1){
            if(id2==nd-1){
                pre=DDcost[permD[id1-1]][permD[id1]]+DPcost[permD[id2]][permP[ti]];
                nex=DDcost[permD[id1-1]][permD[id2]]+DPcost[permD[id1]][permP[ti]];
            }
            else if(id1==0){
                pre=DPcost[permD[id1]][permP[ti]]+DDcost[permD[id2]][permD[id2+1]];
                nex=DPcost[permD[id2]][permP[ti]]+DDcost[permD[id1]][permD[id2+1]];
            }
            else{
                pre=DDcost[permD[id1-1]][permD[id1]]+DDcost[permD[id2]][permD[id2+1]];
                nex=DDcost[permD[id1-1]][permD[id2]]+DDcost[permD[id1]][permD[id2+1]];
            }
        }
        else{
            if(id1==0&&id2==nd-1){
                pre=DPcost[permD[id1]][permP[ti]]+DDcost[permD[id1]][permD[id1+1]]
                    +DDcost[permD[id2-1]][permD[id2]]+DPcost[permD[id2]][permP[ti]];
                nex=DPcost[permD[id2]][permP[ti]]+DDcost[permD[id2]][permD[id1+1]]
                    +DDcost[permD[id2-1]][permD[id1]]+DPcost[permD[id1]][permP[ti]];
            }
            else if(id2==nd-1){
                pre=DDcost[permD[id1-1]][permD[id1]]+DDcost[permD[id1]][permD[id1+1]]
                    +DDcost[permD[id2-1]][permD[id2]]+DPcost[permD[id2]][permP[ti]];
                nex=DDcost[permD[id1-1]][permD[id2]]+DDcost[permD[id2]][permD[id1+1]]
                    +DDcost[permD[id2-1]][permD[id1]]+DPcost[permD[id1]][permP[ti]];
            }
            else if(id1==0){
                pre=DPcost[permD[id1]][permP[ti]]+DDcost[permD[id1]][permD[id1+1]]
                    +DDcost[permD[id2-1]][permD[id2]]+DDcost[permD[id2]][permD[id2+1]];
                nex=DPcost[permD[id2]][permP[ti]]+DDcost[permD[id2]][permD[id1+1]]
                    +DDcost[permD[id2-1]][permD[id1]]+DDcost[permD[id1]][permD[id2+1]];
            }
            else{
                pre=DDcost[permD[id1-1]][permD[id1]]+DDcost[permD[id1]][permD[id1+1]]
                    +DDcost[permD[id2-1]][permD[id2]]+DDcost[permD[id2]][permD[id2+1]];
                nex=DDcost[permD[id1-1]][permD[id2]]+DDcost[permD[id2]][permD[id1+1]]
                    +DDcost[permD[id2-1]][permD[id1]]+DDcost[permD[id1]][permD[id2+1]];
            }
        }

        double diff=pre-nex;

        if(diff>=0||diff>LN[rnd.nextInt()&65535]*ts){
            swap(permD[id1],permD[id2]);
            curscore-=diff;
            if(curscore<bestscore){
                bestscore=curscore;
                //bpermD[ti]=permD;
                if(flag) fpermD[ti]=permD;
                else bpermD[ti]=permD;
            }
        }
        }
        else{
            while(1){
                id1=rnd.nextInt(nd),id2=rnd.nextInt(10)+(id1-id1%11);
                if(id2<nd&&permD[id1]<M&&permD[id2]<M&&id1!=id2) break;
            }
            if(id1>id2) swap(id1,id2);

            if(id2==nd-1&&id1==0){
                pre=DPcost[permD[id1]][permP[ti]]+DPcost[permD[id2]][permP[ti]];
                nex=DPcost[permD[id2]][permP[ti]]+DPcost[permD[id1]][permP[ti]];
            }
            else if(id2==nd-1){
                pre=DDcost[permD[id1-1]][permD[id1]]+DPcost[permD[id2]][permP[ti]];
                nex=DDcost[permD[id1-1]][permD[id2]]+DPcost[permD[id1]][permP[ti]];
            }
            else if(id1==0){
                pre=DPcost[permD[id1]][permP[ti]]+DDcost[permD[id2]][permD[id2+1]];
                nex=DPcost[permD[id2]][permP[ti]]+DDcost[permD[id1]][permD[id2+1]];
            }
            else{
                pre=DDcost[permD[id1-1]][permD[id1]]+DDcost[permD[id2]][permD[id2+1]];
                nex=DDcost[permD[id1-1]][permD[id2]]+DDcost[permD[id1]][permD[id2+1]];
            }


            double diff=pre-nex;

            if(diff>=0||diff>LN[rnd.nextInt()&65535]*ts){
                reverse(permD.begin()+id1,permD.begin()+(id2+1));
                curscore-=diff;
                if(curscore<bestscore){
                    bestscore=curscore;
                    if(flag) fpermD[ti]=permD;
                    else bpermD[ti]=permD;
                }
            }
        }

    }
    /*
    curscore=0;
    curscore+=DPcost[fpermD[ti][0]][fpermP[ti]];
    FOR(i,1,nd){
        curscore+=DDcost[fpermD[ti][i-1]][fpermD[ti][i]];
    }
    curscore+=DPcost[fpermD[ti][nd-1]][permP[ti]];
    cerr<<curscore<<endl;
    cerr<<bestscore<<endl;
    */
}

void improve(int ti){
    pdd score=calcScore2();
    cerr<<score.first<<" "<<score.second<<endl;
    double curspend=score.first,curscore=score.second,bestscore=curscore,bestspend;
    double t,start=timer.get(),invtl=1.0/10;
    ll turn=0;
    vector<int> cand;
    double starttemp=175,endtemp=0.05,ts;
    while(1){
        ++turn;
        t=timer.get();
        if(t-start>10){
            cerr<<turn<<" "<<finalscore<<endl;
            break;
        }
        ts=starttemp+(endtemp-starttemp)*(t-start)*invtl;
        //ts=endtemp+0.5*(starttemp-endtemp)*(1.0+cos((t-start)*invtl*3.1415926535));

        if(bestscore==-1&&delay.size()>0){
            int sz=delay.size();
            int id=rnd.nextInt(sz);
            int pid=delay[id]/M,did=delay[id]%M,nid=fpermD[pid][did];
            double mn=100000;
            int src=-1;
            if(Dlim[nid]==120){
                rep(j,intime120)if(j!=pid){
                    rep(k,fpermD[j].size()){
                        if(fpermD[j][k]<M&&mn>DDcost[fpermD[j][k]][nid]){
                            mn=DDcost[fpermD[j][k]][nid];
                            src=j;
                        }
                        else if(fpermD[j][k]>=M&&mn>DDcost[nid][fpermD[j][k]-M]){
                            mn=DDcost[nid][fpermD[j][k]-M];
                            src=j;
                        }
                    }
                }
            }
            else{
                rep(j,intime240)if(j!=pid){
                    rep(k,fpermD[j].size()){
                        if(fpermD[j][k]<M&&mn>DDcost[fpermD[j][k]][nid]){
                            mn=DDcost[fpermD[j][k]][nid];
                            src=j;
                        }
                        else if(fpermD[j][k]>=M&&mn>DDcost[nid][fpermD[j][k]-M]){
                            mn=DDcost[nid][fpermD[j][k]-M];
                            src=j;
                        }
                    }
                }
            }
            if(src==-1) continue;
            //fpermD[pid].erase(fpermD[pid].begin()+did);
            //fpermD[src].pb(nid);
            permD1=fpermD[pid];
            fpermD[pid].clear();
            rep(i,permD1.size()){
                if(fpermD[pid].size()%11==10) fpermD[pid].pb(fpermP[pid]+M);
                if(i!=did&&permD1[i]<M) fpermD[pid].pb(permD1[i]);
            }
            if(fpermD[pid].size()>0&&fpermD[pid].back()>=M) fpermD[pid].pop_back();

            if(fpermD[src].size()%11==10){
                fpermD[src].pb(fpermP[src]+M);
            }
            fpermD[src].pb(nid);
            permD=fpermD[src];
            
            if(fpermD[pid].size()==0){
                fpermP.erase(fpermP.begin()+pid);
                fpermD.erase(fpermD.begin()+pid);
                fpermD.resize(100);--NP;
                if(pid<src) --src;
            }
            permP=fpermP;
            
            if(fpermD[src].size()<=10){
                solve_Dtsp(src,true);
            }
            else{
                solve_Dvrp(src,true);
            }
            permD.clear();
            if(fpermD[src].size()>10+DIVB){
                copy(fpermD[src].begin()+11,fpermD[src].end(),back_inserter(permD));
                fpermD.insert(fpermD.begin()+(src+1),permD);
                fpermD[src].resize(10);
                fpermP.insert(fpermP.begin()+(src+1),fpermP[src]);
                ++NP;
            }
            pdd score=calcScore2();
            cerr<<score.first<<" "<<score.second<<endl;
            curspend=score.first,curscore=score.second;
            bestspend=curspend;
            
        }
        else{
            //break;
            int sel=rnd.nextInt(100);
            if(sel<75){
                int id1=rnd.nextInt(fpermP.size()),id2=-1,did,nid;
                permD1=fpermD[id1];
                if(fpermD[id1].size()>10&&sel==0){
                    permD=fpermD[id1];
                    permP=fpermP;
                    solve_Dvrp(id1,true);
                    pdd score=calcScore2();
                    double diff=score.second-curscore;
                    if(score.second!=-1&&score.second>=curscore){
                        curscore=score.second;
                        curspend=score.first;
                        if(curscore>finalscore){

                            finalscore=curscore;
                            bestspend=curspend;
                            ffpermP=fpermP;
                            ffpermD=fpermD;
                            fNP=NP;
                        }
                    }
                    else{
                        fpermD[id1]=permD1;
                        calcScore2();
                    }
                }
                else if(fpermD[id1].size()<=2){
                    while(1){
                        id2=rnd.nextInt(fpermP.size());
                        if(id1!=id2) break;
                    }
                    cpermD=fpermD;
                    cpermP=fpermP;
                    int preid=fpermP[id1];
                    permD1=fpermD[id1],permD2=fpermD[id2];
                    rep(j,fpermD[id1].size()){
                        if(fpermD[id2].size()%11==10) fpermD[id2].pb(fpermP[id2]+M);
                        if(fpermD[id1][j]<M) fpermD[id2].pb(fpermD[id1][j]);
                    }
                    
                    
                    fpermP.erase(fpermP.begin()+id1);
                    fpermD.erase(fpermD.begin()+id1);
                    fpermD.resize(100);
                    --NP;
                    if(id1>id2) did=id2;
                    else did=id2-1;
                    permD=fpermD[did];
                    permP=fpermP;
                    if(permD.size()<=10)solve_Dtsp(did,true);
                    else solve_Dvrp(did,true);

                    
                    permD.clear();
                    if(fpermD[did].size()>10+DIVB){
                        copy(fpermD[did].begin()+11,fpermD[did].end(),back_inserter(permD));
                        fpermD.insert(fpermD.begin()+(did+1),permD);
                        fpermD[did].resize(10);
                        fpermP.insert(fpermP.begin()+(did+1),fpermP[did]);
                        ++NP;
                    }

                    pdd score=calcScore2();
                    double diff=score.second-curscore;
                    if(score.second!=-1&&score.second>=curscore){
                    
                    //if(score.second!=-1&&(diff>=0||diff>LN[rnd.nextInt()&65535]*ts)){
                        curscore=score.second;
                        curspend=score.first;
                        if(curscore>finalscore){

                            finalscore=curscore;
                            bestspend=curspend;
                            ffpermP=fpermP;
                            ffpermD=fpermD;
                            fNP=NP;
                        }
                    }
                    else{
                        fpermD=cpermD;
                        fpermP=cpermP;
                        NP=fpermP.size();
                        calcScore2();
                    }
                }
                else{
                    //continue;
                    while(1){
                        did=rnd.nextInt(fpermD[id1].size());
                        if(fpermD[id1][did]<M) break;
                    }
                    nid=fpermD[id1][did];
                    int lim;
                    if(Dlim[nid]==120) lim=intime120;
                    else if(Dlim[nid]==240) lim=intime240;
                    else lim=fpermP.size();
                    rep(i,10){
                    //while(1){
                        id2=rnd.nextInt(lim);
                        if(id1!=id2) break;
                    }
                    if(id2==-1) continue;
                    permD1=fpermD[id1],permD2=fpermD[id2];
                    cpermD=fpermD;cpermP=fpermP;
                    //fpermD[id1].erase(fpermD[id1].begin()+did);
                    fpermD[id1].clear();
                    rep(i,permD1.size()){
                        if(fpermD[id1].size()%11==10) fpermD[id1].pb(fpermP[id1]+M);
                        if(i!=did&&permD1[i]<M) fpermD[id1].pb(permD1[i]);
                    }
                    if(fpermD[id1].size()>0&&fpermD[id1].back()>=M) fpermD[id1].pop_back();

                    if(fpermD[id2].size()%11==10){
                        fpermD[id2].pb(fpermP[id2]+M);
                    }
                    fpermD[id2].pb(nid);

                    permD=fpermD[id2];
                    permP=fpermP;
                    if(fpermD[id2].size()<=10)solve_Dtsp(id2,true);
                    else solve_Dvrp(id2,true);

                    
                    permD.clear();
                    if(fpermD[id2].size()>10+DIVB){
                        copy(fpermD[id2].begin()+11,fpermD[id2].end(),back_inserter(permD));
                        fpermD.insert(fpermD.begin()+(id2+1),permD);
                        fpermD[id2].resize(10);
                        fpermP.insert(fpermP.begin()+(id2+1),fpermP[id2]);
                        ++NP;
                    }

                    pdd score=calcScore2();
                    double diff=score.second-curscore;
                    if(score.second!=-1&&(diff>=0||diff>LN[rnd.nextInt()&65535]*ts)){
                    //if(score.second!=-1&&score.second>=curscore){
                    
                        curscore=score.second;
                        curspend=score.first;
                        
                        if(curscore>finalscore){
                            finalscore=curscore;
                            ffpermP=fpermP;
                            ffpermD=fpermD;
                            fNP=NP;
                        }
                    }
                    else{
                        fpermD=cpermD;
                        fpermP=cpermP;
                        NP=fpermP.size();
                        calcScore2();
                    }
                }
            }
            else if(sel<80||NP<=2){
                //change p
                int id=rnd.nextInt(NP),pid=fpermP[id];
                int id2=neiP[pid][rnd.nextInt(3)];
                fpermP[id]=id2;
                permD1=fpermD[id];
                rep(i,fpermD[id].size())if(fpermD[id][i]>=M) fpermD[id][i]=id2+M;
                permD=fpermD[id];
                permP=fpermP;

                if(fpermD[id].size()<=10)solve_Dtsp(id,true);
                else solve_Dvrp(id,true);

                pdd score=calcScore2();
                double diff=score.second-curscore;
                if(score.second!=-1&&(diff>=0||diff>LN[rnd.nextInt()&65535]*ts)){
                //if(score.second!=-1&&score.second>=curscore){
                    curscore=score.second;
                    curspend=score.first;
                    if(curscore>finalscore){
                        finalscore=curscore;
                        ffpermP=fpermP;
                        ffpermD=fpermD;
                        fNP=NP;
                    }
                }
                else{
                    fpermP[id]=pid,fpermD[id]=permD1;
                    calcScore2();
                }
            }
            else if(sel<92){
                int id=rnd.nextInt(NP),pid=fpermP[id];
                int id2=neiP[pid][rnd.nextInt(5)];
                cpermD=fpermD,cpermP=fpermP;
                permP=fpermP;
                cand.clear();
                rep(i,NP)if(fpermP[i]==pid) cand.pb(i);
                int bd=rnd.nextInt(cand.size());
                if(turn&1){
                    rep(ii,bd+1){
                        int i=cand[ii];
                        if(fpermP[i]==pid){
                            permP[i]=id2;fpermP[i]=id2;
                            rep(j,fpermD[i].size()){
                                if(fpermD[i][j]>=M) fpermD[i][j]=id2+M;
                            }
                            permD=fpermD[i];
                            if(fpermD[i].size()<=10)solve_Dtsp(i,true);
                            else solve_Dvrp(i,true);
                        }
                    
                    }
                }
                else{
                    for(int ii=cand.size()-1;ii>=bd;--ii){
                        int i=cand[ii];
                        if(fpermP[i]==pid){
                            permP[i]=id2;fpermP[i]=id2;
                            rep(j,fpermD[i].size()){
                                if(fpermD[i][j]>=M) fpermD[i][j]=id2+M;
                            }
                            permD=fpermD[i];
                            if(fpermD[i].size()<=10)solve_Dtsp(i,true);
                            else solve_Dvrp(i,true);
                        }
                    }
                    
                }
                
                pdd score=calcScore2();
                double diff=score.second-curscore;
                if(score.second!=-1&&(diff>=0||diff>LN[rnd.nextInt()&65535]*ts)){
                //if(score.second!=-1&&score.second>=curscore){
                    curscore=score.second;
                    curspend=score.first;
                    if(curscore>finalscore){
                        finalscore=curscore;
                        ffpermP=fpermP;
                        ffpermD=fpermD;
                        fNP=NP;
                    }
                }
                else{
                    fpermD=cpermD;
                    fpermP=cpermP;
                    NP=fpermP.size();
                    calcScore2();
                }
            }
            else{
                int id1,id2;
                while(1){
                    id1=rnd.nextInt(NP),id2=rnd.nextInt(NP);
                    if(id1!=id2) break;
                }
                swap(fpermP[id1],fpermP[id2]);
                swap(fpermD[id1],fpermD[id2]);
                pdd score=calcScore2();
                double diff=score.second-curscore;
                if(score.second!=-1&&(diff>=0||diff>LN[rnd.nextInt()&65535]*ts)){
                //if(score.second!=-1&&score.second>=curscore){
                
                    curscore=score.second;
                    curspend=score.first;

                    if(curscore>finalscore){
                        finalscore=curscore;
                        ffpermP=fpermP;
                        ffpermD=fpermD;
                        fNP=NP;
                    }
                }
                else{
                    swap(fpermP[id1],fpermP[id2]);
                    swap(fpermD[id1],fpermD[id2]);
                    calcScore2();
                }
            }
            //tsp vrp
        }

    }
}
void improve2(){
    pdd score=calcScore2();
    cerr<<score.first<<" "<<score.second<<endl;
    double curspend=score.first,curscore=score.second,bestscore=curscore,bestspend;
    double t,start=timer.get();
    ll turn=0;
    vector<int> cand;
    int cint120,cint240;
    while(1){
        ++turn;
        t=timer.get();
        if(t>timeLimit){
            cerr<<turn<<" "<<finalscore<<endl;
            break;
        }

        //npermP=fpermP,npermD=fpermD;
        //!
        calcScore2();
        cint120=intime120,cint240=intime240;
        if(bestscore==-1&&delay.size()>0){
            int sz=delay.size();
            int id=rnd.nextInt(sz);
            int pid=delay[id]/M,did=delay[id]%M,nid=fpermD[pid][did];
            double mn=100000;
            int src=-1;
            if(Dlim[nid]==120){
                rep(j,cint120)if(j!=pid){
                    rep(k,fpermD[j].size()){
                        if(fpermD[j][k]<M&&mn>DDcost[fpermD[j][k]][nid]){
                            mn=DDcost[fpermD[j][k]][nid];
                            src=j;
                        }
                        else if(fpermD[j][k]>=M&&mn>DDcost[nid][fpermD[j][k]-M]){
                            mn=DDcost[nid][fpermD[j][k]-M];
                            src=j;
                        }
                    }
                }
            }
            else{
                rep(j,cint240)if(j!=pid){
                    rep(k,fpermD[j].size()){
                        if(fpermD[j][k]<M&&mn>DDcost[fpermD[j][k]][nid]){
                            mn=DDcost[fpermD[j][k]][nid];
                            src=j;
                        }
                        else if(fpermD[j][k]>=M&&mn>DDcost[nid][fpermD[j][k]-M]){
                            mn=DDcost[nid][fpermD[j][k]-M];
                            src=j;
                        }
                    }
                }
            }
            if(src==-1) continue;
            //fpermD[pid].erase(fpermD[pid].begin()+did);
            //fpermD[src].pb(nid);
            permD1=fpermD[pid];
            fpermD[pid].clear();
            rep(i,permD1.size()){
                if(fpermD[pid].size()%11==10) fpermD[pid].pb(fpermP[pid]+M);
                if(i!=did&&permD1[i]<M) fpermD[pid].pb(permD1[i]);
            }
            if(fpermD[pid].size()>0&&fpermD[pid].back()>=M) fpermD[pid].pop_back();

            if(fpermD[src].size()%11==10){
                fpermD[src].pb(fpermP[src]+M);
            }
            fpermD[src].pb(nid);
            permD=fpermD[src];
            
            if(fpermD[pid].size()==0){
                fpermP.erase(fpermP.begin()+pid);
                fpermD.erase(fpermD.begin()+pid);
                fpermD.resize(100);--NP;
                if(pid<src) --src;
            }
            permP=fpermP;
            
            if(fpermD[src].size()<=10){
                solve_Dtsp(src,true);
            }
            else{
                solve_Dvrp(src,true);
            }
            permD.clear();
            if(fpermD[src].size()>10+DIVB){
                copy(fpermD[src].begin()+11,fpermD[src].end(),back_inserter(permD));
                fpermD.insert(fpermD.begin()+(src+1),permD);
                fpermD[src].resize(10);
                fpermP.insert(fpermP.begin()+(src+1),fpermP[src]);
                ++NP;
            }
            pdd score=calcScore2();
            cerr<<score.first<<" "<<score.second<<endl;
            curspend=score.first,curscore=score.second;
            bestspend=curspend;
            
        }
        else{ 
        int lp=80.0+(80.0*2-80.0)*(t-start)/7;
        cpermD=fpermD,cpermP=fpermP,cNP=cpermP.size();
        double cbestscore=-2;
        rep(_,lp){
            fpermD=cpermD,fpermP=cpermP,NP=cNP;
            int sel=rnd.nextInt(100);
            if(sel<70){
                int id1=rnd.nextInt(fpermP.size()),id2=-1,did,nid;
                permD1=fpermD[id1];
                //if(fpermD[id1].size()>10&&sel<=1){
                if(sel==0){
                    permD=fpermD[id1];
                    permP=fpermP;
                    if(fpermD[id1].size()>10){
                        solve_Dvrp(id1,true);
                    }
                    else{
                        solve_Dtsp(id1,true);
                    }
                    pdd score=calcScore2();
                    if(score.second>cbestscore){
                        cbestscore=score.second;
                        npermP=fpermP,npermD=fpermD,nNP=NP;
                        if(cbestscore>finalscore){

                            finalscore=cbestscore;;
                            ffpermP=fpermP;
                            ffpermD=fpermD;
                            fNP=NP;
                        }
                    }
                }
                else if(fpermD[id1].size()<=2){
                    while(1){
                        id2=rnd.nextInt(fpermP.size());
                        if(id1!=id2) break;
                    }
                    cpermD=fpermD;
                    cpermP=fpermP;
                    int preid=fpermP[id1];
                    permD1=fpermD[id1],permD2=fpermD[id2];
                    rep(j,fpermD[id1].size()){
                        if(fpermD[id2].size()%11==10) fpermD[id2].pb(fpermP[id2]+M);
                        if(fpermD[id1][j]<M) fpermD[id2].pb(fpermD[id1][j]);
                    }
                    
                    
                    fpermP.erase(fpermP.begin()+id1);
                    fpermD.erase(fpermD.begin()+id1);
                    fpermD.resize(100);
                    --NP;
                    if(id1>id2) did=id2;
                    else did=id2-1;
                    permD=fpermD[did];
                    permP=fpermP;
                    if(permD.size()<=10)solve_Dtsp(did,true);
                    else solve_Dvrp(did,true);

                    
                    permD.clear();
                    if(fpermD[did].size()>10+DIVB){
                        copy(fpermD[did].begin()+11,fpermD[did].end(),back_inserter(permD));
                        fpermD.insert(fpermD.begin()+(did+1),permD);
                        fpermD[did].resize(10);
                        fpermP.insert(fpermP.begin()+(did+1),fpermP[did]);
                        ++NP;
                    }

                    pdd score=calcScore2();
                    if(score.second>cbestscore){
                        cbestscore=score.second;
                        npermP=fpermP,npermD=fpermD,nNP=NP;
                        if(cbestscore>finalscore){

                            finalscore=cbestscore;;
                            ffpermP=fpermP;
                            ffpermD=fpermD;
                            fNP=NP;
                        }
                    }
                }
                else{
                    //continue;
                    while(1){
                        did=rnd.nextInt(fpermD[id1].size());
                        if(fpermD[id1][did]<M) break;
                    }
                    nid=fpermD[id1][did];
                    int lim;
                    if(Dlim[nid]==120) lim=cint120;
                    else if(Dlim[nid]==240) lim=cint240;
                    else lim=fpermP.size();
                    rep(i,10){
                    //while(1){
                        id2=rnd.nextInt(lim);
                        if(id1!=id2) break;
                    }
                    if(id2==-1) continue;
                    permD1=fpermD[id1],permD2=fpermD[id2];
                    cpermD=fpermD;cpermP=fpermP;
                    //fpermD[id1].erase(fpermD[id1].begin()+did);
                    fpermD[id1].clear();
                    rep(i,permD1.size()){
                        if(fpermD[id1].size()%11==10) fpermD[id1].pb(fpermP[id1]+M);
                        if(i!=did&&permD1[i]<M) fpermD[id1].pb(permD1[i]);
                    }
                    if(fpermD[id1].size()>0&&fpermD[id1].back()>=M) fpermD[id1].pop_back();

                    if(fpermD[id2].size()%11==10){
                        fpermD[id2].pb(fpermP[id2]+M);
                    }
                    fpermD[id2].pb(nid);

                    permD=fpermD[id2];
                    permP=fpermP;
                    if(fpermD[id2].size()<=10)solve_Dtsp(id2,true);
                    else solve_Dvrp(id2,true);

                    
                    permD.clear();
                    if(fpermD[id2].size()>10+DIVB){
                        copy(fpermD[id2].begin()+11,fpermD[id2].end(),back_inserter(permD));
                        fpermD.insert(fpermD.begin()+(id2+1),permD);
                        fpermD[id2].resize(10);
                        fpermP.insert(fpermP.begin()+(id2+1),fpermP[id2]);
                        ++NP;
                    }

                    pdd score=calcScore2();
                    if(score.second>cbestscore){
                        cbestscore=score.second;
                        npermP=fpermP,npermD=fpermD,nNP=NP;
                        if(cbestscore>finalscore){

                            finalscore=cbestscore;;
                            ffpermP=fpermP;
                            ffpermD=fpermD;
                            fNP=NP;
                        }
                    }
                }
            }
            else if(sel<80||NP<=2){
                //change p
                int id=rnd.nextInt(NP),pid=fpermP[id];
                int id2=neiP[pid][rnd.nextInt(3)];
                fpermP[id]=id2;
                permD1=fpermD[id];
                rep(i,fpermD[id].size())if(fpermD[id][i]>=M) fpermD[id][i]=id2+M;
                permD=fpermD[id];
                permP=fpermP;

                if(fpermD[id].size()<=10)solve_Dtsp(id,true);
                else solve_Dvrp(id,true);

                pdd score=calcScore2();
                if(score.second>cbestscore){
                    cbestscore=score.second;
                    npermP=fpermP,npermD=fpermD,nNP=NP;
                    if(cbestscore>finalscore){

                        finalscore=cbestscore;;
                        ffpermP=fpermP;
                        ffpermD=fpermD;
                        fNP=NP;
                    }
                }
            }
            else if(sel<91){
                int id=rnd.nextInt(NP),pid=fpermP[id];
                int id2=neiP[pid][rnd.nextInt(5)];
                cpermD=fpermD,cpermP=fpermP;
                permP=fpermP;
                
                cand.clear();
                rep(i,NP)if(fpermP[i]==pid) cand.pb(i);
                int bd=rnd.nextInt(cand.size());
                if(turn&1){
                    rep(ii,bd+1){
                        int i=cand[ii];
                        if(fpermP[i]==pid){
                            permP[i]=id2;fpermP[i]=id2;
                            rep(j,fpermD[i].size()){
                                if(fpermD[i][j]>=M) fpermD[i][j]=id2+M;
                            }
                            permD=fpermD[i];
                            if(fpermD[i].size()<=10)solve_Dtsp(i,true);
                            else solve_Dvrp(i,true);
                        }
                    
                    }
                }
                else{
                    for(int ii=cand.size()-1;ii>=bd;--ii){
                        int i=cand[ii];
                        if(fpermP[i]==pid){
                            permP[i]=id2;fpermP[i]=id2;
                            rep(j,fpermD[i].size()){
                                if(fpermD[i][j]>=M) fpermD[i][j]=id2+M;
                            }
                            permD=fpermD[i];
                            if(fpermD[i].size()<=10)solve_Dtsp(i,true);
                            else solve_Dvrp(i,true);
                        }
                    }
                    
                }
                pdd score=calcScore2();
                double diff=score.second-curscore;
                if(score.second>cbestscore){
                    cbestscore=score.second;
                    npermP=fpermP,npermD=fpermD,nNP=NP;
                    if(curscore>finalscore){
                        finalscore=curscore;
                        ffpermP=fpermP;
                        ffpermD=fpermD;
                        fNP=NP;
                    }
                }
            }
            else{
                int id1,id2;
                while(1){
                    id1=rnd.nextInt(NP),id2=rnd.nextInt(NP);
                    if(id1!=id2) break;
                }
                swap(fpermP[id1],fpermP[id2]);
                swap(fpermD[id1],fpermD[id2]);
                pdd score=calcScore2();
                if(score.second>cbestscore){
                    cbestscore=score.second;
                    npermP=fpermP,npermD=fpermD,nNP=NP;
                    if(cbestscore>finalscore){

                        finalscore=cbestscore;;
                        ffpermP=fpermP;
                        ffpermD=fpermD;
                        fNP=NP;
                    }
                }
            }
            //tsp vrp
        }
        fpermP=npermP,fpermD=npermD;
        }

    }
}
int main(){
    timer.reset();
    inpt();
    bpermD.resize(100);fpermD.resize(100);
    
    double tmpm=1.0/(2.0*65536);
    rep(i,65536){
        LN[i]=log((double)i/65536+tmpm);
        //LN[i]=max(-4.0,LN[i]);
    }

    //rep(i,M){
    for(int i:tg1){
        double mn=100000;
        int nx=-1;
        rep(j,N)if(ufP[j]){
            if(mn>DPcost[i][j]){
                mn=DPcost[i][j];
                nx=j;
            }
            
        }
        ++cntp[nx];
        candD[nx].pb(i);
        if(revP[nx]!=-1) candD[revP[nx]].pb(i);
    }
    rep(i,N)if(cntp[i]>0) candP.pb(cntp[i],i),usedP[i]=true;
    sort(candP.begin(),candP.end(),greater<pint>());
    NP=candP.size();
    bpermP.resize(NP);
    rep(i,NP) bpermP[i]=candP[i].second;


    //remove small P size
    
    for(int ii=NP-1;ii>=0;--ii){
        int i=bpermP[ii];
        if(candD[i].size()==1){
            double pdist=DPcost[candD[i][0]][i]*2;
            double mn=100000;
            int src=-1;
            rep(jj,NP)if(ii!=jj){
                int j=bpermP[jj];
                rep(k,candD[j].size()){
                    if(mn>DDcost[candD[j][k]][candD[i][0]]){
                        mn=DDcost[candD[j][k]][candD[i][0]];
                        src=j;
                    }
                }
            }
            if(src!=-1&&mn-3<pdist){
                bpermP.erase(bpermP.begin()+ii);--NP;
                candD[src].pb(candD[i][0]);
                if(revP[src]!=-1) candD[revP[src]].pb(candD[i][0]);
                candD[i].clear();
                if(revP[i]!=-1) candD[revP[i]].clear();

            }
        }
    }
    
    NP=bpermP.size();

    permP=bpermP;
    double pdist;
    if(NP>0)pdist=solve_Ptsp();
    bpermP120=bpermP;
    NP120=NP;

    //2opt or simple_vrp
    //revP swap
    rep(i,NP){
        int nxp=bpermP120[i];
        if(candD[nxp].size()>0){
            
            if(candD[nxp].size()<=10){
                bpermD[i]=candD[nxp];
                permD=bpermD[i];
                solve_Dtsp(i);
            }
            else{
                
                bpermD[i]=candD[nxp];
                int sz=candD[nxp].size();
                //int sep=(sz-1)/10+1;
                //sep=(sz+sep-1)/sep;
                int sep=10;
                permD.clear();
                rep(j,sz){
                    if(j>0&&j%sep==0) permD.pb(nxp+M);
                    permD.pb(candD[nxp][j]);
                }
                solve_Dvrp(i);
            }
        }
    }
    
    pdd score=calcScore();
    cerr<<score.second<<endl;
    totcost=score.first;

    //score test
    /*
    NP=3;
    bpermP={1,0,4};
    bpermD[0]={4,1};
    bpermD[1]={0,3,7,8,11,14,10,9,5,6,-1,2};
    bpermD[2]={13,12};
    score=calcScore();
    cerr<<score<<endl;
    */
    /*
    rep(i,NP){
        cerr<<bpermP[i]<<" ";
        rep(j,bpermD[i].size()){
            cerr<<bpermD[i][j]<<" ";
        }
        cerr<<endl;
    }
    */
    
    if(score.second>14400){
        int cnt=0;
        rep(_,50){
            double mn=100000;
            int nxd=-1,tp,tpos;
            for(int i:tg2){
                rep(j,NP){
                    int sz=bpermD[j].size(),jp=bpermP[j];
                    double nx;
                    if(sz%10!=0){
                        rep(k,sz+1){
                            if(k==0){
                                nx=DPcost[i][jp]+DDcost[bpermD[j][0]][i]-DPcost[bpermD[j][0]][jp]+Dcost[i];
                                
                            }
                            else if(k==sz){
                                nx=DPcost[i][jp]+DDcost[bpermD[j][sz-1]][i]-DPcost[bpermD[j][sz-1]][jp]+Dcost[i];
                                
                            }
                            else{
                                nx=DDcost[bpermD[j][k-1]][i]+DDcost[bpermD[j][k]][i]-DDcost[bpermD[j][k-1]][bpermD[j][k]]+Dcost[i];    
                            }
                            if(mn>nx){
                                mn=nx,nxd=i,tp=j,tpos=k;
                            }
                        }
                    }
                    else{
                        
                        rep(k,sz+1){
                            if(k==0){
                                nx=DPcost[i][jp]+DDcost[bpermD[j][0]][i]-DPcost[bpermD[j][0]][jp]
                                -DDcost[bpermD[j][sz-2]][bpermD[j][sz-1]]+DPcost[bpermD[j][sz-2]][jp]+DPcost[bpermD[j][sz-1]][jp]+Dcost[i];
                                
                            }
                            else if(k==sz){
                                nx=DPcost[i][jp]*2+Dcost[i];
                                
                            }
                            else if(k!=sz-1){
                                nx=DPcost[i][jp]+DDcost[bpermD[j][0]][i]-DPcost[bpermD[j][0]][jp]
                                -DDcost[bpermD[j][sz-2]][bpermD[j][sz-1]]+DPcost[bpermD[j][sz-2]][jp]+DPcost[bpermD[j][sz-1]][jp]+Dcost[i];
                                
                            }
                            if(mn>nx){
                                mn=nx,nxd=i,tp=j,tpos=k;
                            }
                        }
                    }
                }
            }
            if(bpermD[tp].size()>=10){
                preD=bpermD[tp];

                bpermD[tp].insert(bpermD[tp].begin()+tpos,nxd);
                permD1=bpermD[tp];
                bpermD[tp].clear();
                rep(i,permD1.size()){
                    if(bpermD[tp].size()%11==10) bpermD[tp].pb(bpermP[tp]+M);
                    if(permD1[i]<M) bpermD[tp].pb(permD1[i]);
                }
                /*
                if(bpermD[tp].size()%11==0){
                    int pos10=bpermD[tp].size()-1;
                    bpermD[tp].insert(bpermD[tp].begin()+pos10,bpermP[tp]+M);
                }
                rep(k,bpermD[tp].size()){
                    if(k%11==10){
                        if(bpermD[tp][k-1]>=M) swap(bpermD[tp][k],bpermD[tp][k-1]);
                        else if(k+1<bpermD[tp].size()&&bpermD[tp][k+1]>=M) swap(bpermD[tp][k],bpermD[tp][k+1]);
                    }
                }
                */
                score=calcScore();
                cerr<<score.first<<endl;
                if(score.first>120){
                    bpermD[tp]=preD;
                    break;
                }
                ++cnt;
                tg2.erase(nxd);
            }
            else{
            if(score.first+mn<120){
                cerr<<score.first+mn<<endl;
                
                bpermD[tp].insert(bpermD[tp].begin()+tpos,nxd);
                permD1=bpermD[tp];
                bpermD[tp].clear();
                rep(i,permD1.size()){
                    if(bpermD[tp].size()%11==10) bpermD[tp].pb(bpermP[tp]+M);
                    if(permD1[i]<M) bpermD[tp].pb(permD1[i]);
                }
                /*
                if(bpermD[tp].size()%11==10){
                    int pos10=bpermD[tp].size()-1;
                    bpermD[tp].insert(bpermD[tp].begin()+pos10,bpermP[tp]+M);
                }

                rep(k,bpermD[tp].size()){
                    if(k%11==10){
                        if(bpermD[tp][k-1]>=M) swap(bpermD[tp][k],bpermD[tp][k-1]);
                        else if(k+1<bpermD[tp].size()&&bpermD[tp][k+1]>=M) swap(bpermD[tp][k],bpermD[tp][k+1]);
                    }
                }
                */
                score=calcScore();
                cerr<<score.first<<endl;
                tg2.erase(nxd);
                ++cnt;
            }
            else break;
            }
        }
        cerr<<score.second<<endl;
        //tsp vrp
        if(cnt>10){
            rep(i,NP){
                if(bpermD[i].size()<=10){
                    permD=bpermD[i];
                    solve_Dtsp(i);
                }
                else{
                    permP=bpermP;
                    permD=bpermD[i];
                    solve_Dvrp(i);
                }
            }
        }
    }
    else if(score.second<0){
        int pid,ppos,dpos;
        while(1){
            while(1){
                pid=rnd.nextInt(NP);
                ppos=bpermP[pid],dpos=rnd.nextInt(bpermD[pid].size());
                if(bpermD[pid][dpos]<M&&Dlim[bpermD[pid][dpos]]==240) break;
            }
            
            //bpermD[pid].erase(bpermD[pid].begin()+dpos);

            tg2.insert(bpermD[pid][dpos]);
            permD1=bpermD[pid];
            bpermD[pid].clear();
            rep(i,permD1.size()){
                if(bpermD[pid].size()%11==10) bpermD[pid].pb(bpermP[pid]+M);
                if(i!=dpos&&permD1[i]<M) bpermD[pid].pb(permD1[i]);
            }
            if(bpermD[pid].size()>0&&bpermD[pid].back()>=M) bpermD[pid].pop_back();
            
            if(bpermD[pid].size()==0){
                bpermP.erase(bpermP.begin()+pid);
                bpermD.erase(bpermD.begin()+pid);
                bpermD.resize(100);
                --NP;
            }
            score=calcScore();
            if(score.second>0) break;
        }
    }
    NP120=NP;

    //usedD flag
    int numD=0;
    rep(i,NP){
        cerr<<bpermP[i]<<" ";
        rep(j,bpermD[i].size()){
            if(bpermD[i][j]<M&&!usedD[bpermD[i][j]]) usedD[bpermD[i][j]]=true,++numD;
            else cerr<<"ng"<<endl;
            cerr<<bpermD[i][j]<<" ";
        }
        cerr<<endl;
    }
    score=calcScore();
    cerr<<score.first<<" "<<score.second<<endl;

    //240minまで
    tg2.clear();
    rep(i,M){
        if(Dlim[i]==240&&!usedD[i]) ttg3.pb(0,i);
    }
    if(tg2.empty()){
        rep(i,M){
            if(!usedD[i]&&Dlim[i]>240) ttg3.pb(avedist[i],i),++numD;
        }
    }
    else{
        rep(i,M){
            if(!usedD[i]&&Dlim[i]>240) ttg3.pb(avedist[i],i),++numD;
        }
    }
    sort(ttg3.begin(),ttg3.end());
    rep(i,ttg3.size()) tg3.insert(ttg3[i].second);

    rep(i,N) candD[i].clear();
    memset(cntp,0,sizeof(cntp));
    for(int i:tg3)if(!usedD[i]){
        double mn=100000;
        int nx=-1;
        rep(j,N)if(ufP[j]){
            if(mn>DPcost[i][j]){
                mn=DPcost[i][j];
                nx=j;
            }
            
        }
        ++cntp[nx];
        candD[nx].pb(i);
        if(revP[nx]!=-1) candD[revP[nx]].pb(i);
    }

    candP.clear();
    rep(i,N)if(cntp[i]>0) candP.pb(cntp[i],i),usedP[i]=true;
    sort(candP.begin(),candP.end(),greater<pint>());
    //NP=candP.size()+NP120;
    //bpermP.resize(NP);
    //FOR(i,NP120,NP) bpermP[i]=candP[i-NP120].second;
    rep(i,candP.size()) bpermP.pb(candP[i].second);
    NP=bpermP.size();

    //remove small P size
    
    for(int ii=NP-1;ii>=NP120;--ii){
        int i=bpermP[ii];
        if(candD[i].size()==1){
            double pdist=DPcost[candD[i][0]][i]*2;
            double mn=100000;
            int src=-1;
            FOR(jj,NP120,NP)if(ii!=jj){
                int j=bpermP[jj];
                rep(k,candD[j].size()){
                    if(mn>DDcost[candD[j][k]][candD[i][0]]){
                        mn=DDcost[candD[j][k]][candD[i][0]];
                        src=j;
                    }
                }
            }
            if(src!=-1&&mn-3<pdist){
                bpermP.erase(bpermP.begin()+ii);--NP;
                candD[src].pb(candD[i][0]);
                if(revP[src]!=-1) candD[revP[src]].pb(candD[i][0]);
                candD[i].clear();
                if(revP[i]!=-1) candD[revP[i]].clear();

            }
        }
    }
    

    NP=bpermP.size();
    permP=bpermP;
    if(NP120<NP)pdist=solve_Ptsp();


    //2opt or simple_vrp
    //revP swap
    FOR(i,NP120,NP){
        int nxp=bpermP[i];
        if(candD[nxp].size()>0){
            if(candD[nxp].size()<=10){
                bpermD[i]=candD[nxp];
                permD=bpermD[i];
                solve_Dtsp(i);
            }
            else{
                
                //bpermD[i]=candD[nxp];
                int sz=candD[nxp].size();
                //int sep=(sz-1)/10+1;
                //sep=(sz+sep-1)/sep;
                int sep=10;
                //permD.clear();
                rep(j,sz){
                    if(j>0&&j%sep==0) bpermD[i].pb(nxp+M);
                    bpermD[i].pb(candD[nxp][j]);
                }
                permD=bpermD[i];
                solve_Dvrp(i);
            }
        }
    }

    score=calcScore();
    cerr<<score.first<<" "<<score.second<<endl;
    
    numD=0;
    FOR(i,NP120,NP){
        cerr<<bpermP[i]<<" ";
        rep(j,bpermD[i].size()){
            if(!usedD[bpermD[i][j]]) usedD[bpermD[i][j]]=true,++numD;
            else cerr<<"ng"<<endl;
            cerr<<bpermD[i][j]<<" ";
        }
        cerr<<endl;
    }

    //simple localsearch
    int cur=-1;
    rep(i,NP){
        fpermP.pb(bpermP[i]),++cur;
        rep(j,bpermD[i].size()){
            if(fpermD[cur].size()%11==10) fpermP.pb(bpermP[i]),++cur;
            if(bpermD[i][j]<M)fpermD[cur].pb(bpermD[i][j]);
        }  
    }
    NP=cur+1;
    rep(i,NP)if(fpermD[i].size()>10){
        permD=fpermD[i];
        solve_Dvrp(i,true);
    }
    score=calcScore2();
    cerr<<score.first<<" "<<score.second<<endl;


    vector<pint> candnei; 
    rep(i,N){
        candnei.clear();
        rep(j,N)if(i!=j){
            candnei.pb(PPcost[i][j],j);
        }
        sort(candnei.begin(),candnei.end());
        rep(k,5){
            neiP[i].pb(candnei[k].second);
        }
    }

    bpermP=fpermP;
    bpermD=fpermD;
    bNP=NP;
    ffpermP=fpermP;
    ffpermD=fpermD;
    fNP=NP;
    finalscore=score.second;
    rep(i,5){
        fpermP=bpermP;
        fpermD=bpermD;
        NP=bNP;
        improve(i);
    }
    fpermP=ffpermP;
    fpermD=ffpermD;
    NP=fNP;
    improve2();

    fpermP=ffpermP;
    fpermD=ffpermD;
    NP=fNP;
    rep(i,NP){
        cerr<<fpermP[i]<<" ";
        rep(j,fpermD[i].size()){
            if(dup[fpermD[i][j]]&&fpermD[i][j]<M) cerr<<"ng"<<endl;
            cerr<<fpermD[i][j]<<" ",dup[fpermD[i][j]]=true;
        }
        cerr<<endl;
    }
    score=calcScore2();
    cerr<<score.first<<" "<<score.second<<endl;
    
    //output
    int dlim=((int)(finalscore+(0.0001)))/10000,dcnt=0;
    rep(i,NP){
        cout<<PID[fpermP[i]]<<" ";
        rep(j,fpermD[i].size()){
            if(fpermD[i][j]<M){
                cout<<DID[fpermD[i][j]]<<" ";
                ++dcnt;
                if(dcnt==dlim){
                    cout<<endl;
                    return 0;
                }
            }
            else if(j+1!=fpermD[i].size()){
                cout<<endl;
                cout<<PID[fpermD[i][j]-M]<<" ";
            }
        }
        cout<<endl;
    }
    return 0;
}
