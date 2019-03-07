#include <bits/stdc++.h>
using namespace std;
#define FOR(i,a,b) for(int i=(a);i<(b);++i)
#define rep(i,n)   FOR(i,0,n)
#define pb emplace_back
typedef long long ll;
typedef pair<int,int> pint;

typedef pair<double,double> pdb;
double timeLimit=9.9,timeLimit2=14.9;
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
struct Rand2 {
	uint32_t y;
	static constexpr double iDouble=1.0/(1LL<<32);
	Rand2() {
		y=2463534242;
        //y=random_device()();
	}
	uint32_t nextInt() {
    	return y^=(y^=(y^=y<<13)>>17)<<5;
	}
	uint32_t nextInt(uint32_t n) {
		return nextInt()%n;
	}
	double nextDouble() {return nextInt()*iDouble;}
} rnd;

Timer timer;
double LN[65536];
int N,M,depX,depY,L;
int posX[501],posY[501];
int cap[21];
double speed[21];
double dist[501][501];
vector<int> ans[21];
int perm[525],bperm[525],initm[21];
double bestscore=0,bvs=0;
double fac=10;
inline pdb getScore(){
    int cur=0,tcap=0,pre=0,id;
    double ret=0,ret2=0,sm=0;
    FOR(i,1,L+1){
        id=perm[i];
        if(id==0){
            sm+=dist[pre][0]*speed[cur];
            ret=max(ret,sm);
            ret2+=sm;
            sm=0;++cur;
            tcap=0;pre=0;
        }
        else{
            if(tcap==cap[cur]){
                sm+=dist[0][pre]*speed[cur];
                tcap=0;pre=0;
            }
            sm+=dist[pre][id]*speed[cur];
            pre=id;
            ++tcap;  
        }
    }
    return {ret,ret*fac+ret2};
}
pint hl[21];
inline pdb getScore2(){
    int cur=0,tcap=0,pre=0,id;
    double ret=0,ret2=0,sm=0;
    hl[0].first=1;
    FOR(i,1,L+1){
        id=perm[i];
        if(id==0){
            sm+=dist[pre][0]*speed[cur];
            ret=max(ret,sm);
            ret2+=sm;
            hl[cur].second=i;
            sm=0;++cur;
            hl[cur].first=i;
            tcap=0;pre=0;
        }
        else{
            if(tcap==cap[cur]){
                sm+=dist[0][pre]*speed[cur];
                tcap=0;pre=0;
            }
            sm+=dist[pre][id]*speed[cur];
            pre=id;
            ++tcap;  
        }
    }
    return {ret,ret*fac+ret2};
}
inline bool forceupdate(double diff,double temp){
    if(diff>=0) return true;
    return diff*temp>LN[rnd.nextInt()&65535];
    //double d=(diff)*temp;
    //if(d<-6) return false;
    //return exp(d)>rnd.nextDouble();
}
int tperm[525];
void improve(){
    double start=timer.get();
    int turn=0;
    double diff;
    double t=start,invtl=1.0/timeLimit;
    double curscore=bvs;
    double starttemp=30,endtemp=0.1,ts=starttemp;
    double startf=10,endf=300;
    fac=startf;
    while(1){
        ++turn;
        if((turn&15)==0){
            t=timer.get();
            ts=(starttemp+(endtemp-starttemp)*(t-start)*invtl);
            fac=(startf+(endf-startf)*(t-start)*invtl);
        }
        if(t>timeLimit){
            cerr<<turn<<endl;
            return;
        }
        int r,l=rnd.nextInt(L-1)+1;
        //int sel=rnd.nextInt(3);
        /*
        if(sel==0){
            while(1){
                r=rnd.nextInt(L-1)+1;
                if(perm[r]!=0&&r!=l) break;
            }
            memcpy(tperm,perm,sizeof(perm));
            if(r>l){
                int tm=perm[r];
                for(int i=r;i>l;--i){
                    perm[i]=perm[i-1];
                }
                perm[l]=tm;
            }
            else{
                int tm=perm[r];
                for(int i=r+1;i<=l;++i){
                    perm[i-1]=perm[i];
                }
                perm[l]=tm;
            }
            pdb tmp=getScore();
            double nxscore=tmp.second;
            diff=curscore-nxscore;
            if(diff>=0||diff>LN[rnd.nextInt()&65535]*ts){
                curscore=nxscore;
                if(bestscore>tmp.first){
                    bestscore=tmp.first;
                    memcpy(bperm,perm,sizeof(perm));
                }
            }
            else{
                memcpy(perm,tperm,sizeof(perm));
            }
        }
        */
        //else{
        bool flag=false;
        if(perm[l]==0){
            rep(ii,15){
                r=l+rnd.nextInt(7)-3;
                if(r>0&&r<L&&perm[r]!=0){
                    flag=true;
                    break;
                }
            }
        }
        else{
            flag=true;
            while(1){
                r=rnd.nextInt(L-1)+1;
                if(r!=l&&perm[r]!=0) break;
            }
        }
        if(!flag) continue;
        swap(perm[r],perm[l]);
        pdb tmp=getScore();
        double nxscore=tmp.second;
        diff=curscore-nxscore;
        if(diff>=0||diff>LN[rnd.nextInt()&65535]*ts){
            curscore=nxscore;
            if(bestscore>tmp.first){
                bestscore=tmp.first;
                memcpy(bperm,perm,sizeof(perm));
            }
        }
        else{
            swap(perm[r],perm[l]);
        }
        //}
    }
}

void opt2(){
    
    double start=timer.get();
    int turn=0;
    double diff;
    double t=start,invtl=1.0/(timeLimit2-timeLimit);
    double curscore;
    double starttemp=2,endtemp=0.1,ts=starttemp;
    double startf=300,endf=300;
    fac=startf;
    memcpy(perm,bperm,sizeof(perm));
    pdb tmp=getScore2();
    curscore=tmp.second;
    while(1){
        ++turn;
        if((turn&15)==0){
            t=timer.get();
            ts=(starttemp+(endtemp-starttemp)*(t-start)*invtl);
            fac=(startf+(endf-startf)*(t-start)*invtl);
        }
        if(t>timeLimit2){
            cerr<<turn<<endl;
            return;
        }
        int vid;
        while(1){
            vid=rnd.nextInt(M);
            if(hl[vid].second-hl[vid].first>=3) break;
        }
        int width=hl[vid].second-hl[vid].first,l,r;
        while(1){
            l=rnd.nextInt(width-1)+hl[vid].first+1,r=rnd.nextInt(width-1)+hl[vid].first+1;
            if(r!=l) break;
        }
        if(l>r) swap(l,r);
        reverse(perm+l,perm+r+1);
        pdb tmp=getScore();
        double nxscore=tmp.second;
        diff=curscore-nxscore;
        if(diff>=0||diff>LN[rnd.nextInt()&65535]*ts){
            curscore=nxscore;
            if(bestscore>tmp.first){
                bestscore=tmp.first;
                memcpy(bperm,perm,sizeof(perm));
            }
        }
        else{
            reverse(perm+l,perm+r+1);
        }
    }
}
int totsp=0;
int asum[21];
int main() {
    timer.reset();
    cin>>N>>M;
    L=N+M;
    cin>>posX[0]>>posY[0];
    rep(i,N) cin>>posX[i+1]>>posY[i+1];
    rep(i,M){
        cin>>cap[i]>>speed[i];
        speed[i]=1.0/speed[i];
    }
    rep(i,N+1)rep(j,N+1) dist[i][j]=hypot(posX[i]-posX[j],posY[i]-posY[j]);
    /*
    asum[0]=speed[0];
    FOR(i,1,M) asum[i]+=asum[i-1]+speed[i];
    totsp=asum[M-1];
    rep(i,N){
        int tt=rnd.nextInt(totsp);
        rep(j,M){
            if(tt<asum[j]){
                ++initm[j];
                break;
            }
        }
    }
    */
    rep(i,M) initm[i]=N/M+(N%M>i?1:0);
    rep(i,M-1) initm[i+1]+=initm[i];
    int cur=0,id=1,ii=1;
    while(1){
        if(initm[cur]>=id){
            perm[ii]=id;
            ++ii,++id;
        }
        else{
            perm[ii]=0;
            ++ii;++cur;
        }
        if(id==N+1) break;
    }

    pdb tmp=getScore();
    bestscore=tmp.first;
    bvs=tmp.second;
    memcpy(bperm,perm,sizeof(perm));

    cerr<<bestscore<<endl;

    double tmpm=1.0/(2.0*65536);
    rep(i,65536){
        LN[i]=log((double)i/65536+tmpm);
        //LN[i]=max(-6.0,LN[i]);
    }
    improve();

    cerr<<bestscore<<endl;

    opt2();

    cerr<<bestscore<<endl;

    cur=0;
    FOR(i,1,L+1){
        if(bperm[i]==0) ++cur;
        else{
            ans[cur].emplace_back(bperm[i]-1);
        }
    }
    rep(i,M){
        cout<<ans[i].size();
        rep(j,ans[i].size()){
            cout<<" "<<ans[i][j];
        }
        cout<<endl;
    }
    return 0;
    
}