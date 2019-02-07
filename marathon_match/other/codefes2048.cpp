#include <bits/stdc++.h>
#include <random>
using namespace std;
#define FOR(i,a,b) for(int i=(a);i<(b);++i)
#define rep(i,n)   FOR(i,0,n)
#define pb emplace_back
typedef long long ll;
typedef pair<int,int> pint;
typedef pair<ll,ll> pll;

bool ANTI_OF=true;
struct Rand {
	uint32_t x,y,z,w;
	static constexpr double iDouble=1.0/(1LL<<32);
	Rand(uint32_t w):w(w){
		x=123456789;y=362436069;z=521288629;
	}
	uint32_t nextInt(uint32_t n) {
		uint32_t t=x^(x<<11);
		x=y;y=z;z=w;
		w=(w^(w>>19))^(t^(t>>8));
		return w%n;
	}
	uint32_t nextInt() {
		uint32_t t=x^(x<<11);
		x=y;y=z;z=w;
		return w=(w^(w>>19))^(t^(t>>8));
	}
	double nextDouble() {return nextInt()*iDouble;}
} rnd(ANTI_OF?random_device()():88675123);
const ll INF=1000100010001000100;
int pid;
int turn,timeleft,myscore,opscore;
vector<pint> atk;
int ansM,ansV,anscom;
vector<pint> anspos;
int mg[5][5],og[5][5];
const int dx[]={-1,0,1,0},dy[]={0,-1,0,1};
string dir="LDRU";
ll totscore=0;
ll mnv=-INF,mxv=INF;
int atksz;
bool ok;
int space=0;
int lg[65537];
ll simu(int g[5][5],bool me,bool rec);
inline ll getmygridvalue(ll diff,ll cnt,int maxv,int same,int g[5][5],bool rec){
    //if(space<=4) cnt+=2;
    ll ret=((1<<cnt)+same);//+(int)sqrt(point/16);
    if(!rec)ret+=simu(g,false,!rec);
    //ll ret=cnt*3+same-diff-maxv;
    return ret;
}
inline ll getopgridvalue(ll diff,ll cnt,int maxv,int same,int g[5][5],bool rec){
    ll ret=((1<<cnt)+same);//+(int)sqrt(point/16);
    if(!rec)ret+=simu(g,false,!rec);
    //ll ret=cnt*3+same-diff-maxv;
    return ret;
}
int command[4],curV[4],windir;
ll curscore[4],myvalue[4];
bool ng[10];
ll simu(int g[5][5],bool me,bool rec){
    int tg[5][5],rg[5][5];
    ll mx=mnv;
    if(me){
        windir=-1;
        memset(curscore,-1,sizeof(curscore));
    }
    rep(k,4){
        ll point=0;
        int cnt=0;
        memcpy(tg,g,sizeof(tg));
        memcpy(rg,g,sizeof(rg));
        bool up=false;
        rep(i,5){
            int cur=0,pre=tg[i][0];
            FOR(j,1,5){
                if(pre==0){
                    pre=tg[i][j];
                    if(pre!=0) up=true;
                    continue;
                }
                if(pre==tg[i][j]){
                    point+=(1ll<<(pre+1));
                    ++cnt;
                    rg[i][cur++]=pre+1;
                    pre=-1;
                    up=true;
                }
                else{
                    if(pre==-1) pre=tg[i][j];
                    else{
                        if(tg[i][j]!=0){
                            rg[i][cur++]=pre;
                            pre=tg[i][j];
                        }
                    }
                }
            }
            if(pre!=-1) rg[i][cur++]=pre;
            while(cur<5){
                rg[i][cur++]=0;
            }
        }
        ll mxnum=0,mxy=-1,mxx=-1;
        int zero=0,samex=0,samey=0,same=0,diff=0;
        int curx=0,cury=0,maxv=0;
        rep(i,5){
            curx=0,cury=0;
            rep(j,5){
                maxv=max(maxv,rg[i][j]);
                if(rg[i][j]!=tg[i][j]){
                    up=true;
                }
                if(rg[i][j]==0) ++zero;
                else rep(k,4){
                    int tx=j+dx[k],ty=i+dy[k],dist=0;
                    while(tx>=0&&ty>=0&&tx<5&&ty<5&&rg[ty][tx]==0)++dist,tx+=dx[k],ty+=dy[k];
                    if(tx>=0&&ty>=0&&tx<5&&ty<5&&rg[i][j]==rg[ty][tx]){
                        if(dist!=0){
                            if(k&1)++samey;
                            else ++samex;
                        }
                        else{
                            if(k&1)samey+=2;
                            else samex+=2;
                        }
                    }
                    else{
                        if(dist==0)diff+=abs(rg[i][j]-rg[ty][tx]);
                    }
                }
                
            }
        }
        same=max(samex,samey)/2;
        diff/=2;
        ll tmpval=-INF;
        if(me){
            if(up)tmpval=getmygridvalue(diff,cnt,maxv,same,rg,rec);
            if(up&&ok&&(1<<cnt)>=atksz&&!ng[lg[(1<<(cnt+1))/atksz]]){
                tmpval=INF;
                windir=k;
            }
        }
        else tmpval=getopgridvalue(diff,cnt,maxv,same,rg,rec);

        /*
        cerr<<dir[k]<<" "<<tmpval<<" "<<point<<" "<<cnt<<endl;
        rep(i,5){
            rep(j,5){
                if(rg[i][j]!=0)cerr<<(1<<rg[i][j]);
                else cerr<<0;
            }
            cerr<<endl;
        }
        */
        
        if(me)myvalue[k]=tmpval,command[k]=k,curscore[k]=point,curV[k]=cnt;
        else{
            if(up&&mx<tmpval){
                mx=tmpval;
            }
        }
        rep(i,5)rep(j,5){
            g[j][4-i]=tg[i][j];
        }
    }
    rep(i,5)rep(j,5){
        g[j][4-i]=tg[i][j];
    }
    return mx;
}

inline void shuffle(){
    int sz=atk.size();
    rep(i,sz) swap(atk[rnd.nextInt(sz)],atk[rnd.nextInt(sz)]);
    return;
}
int main(){
    /*
    rep(i,5)rep(j,5) cin>>mg[i][j];
    simu(mg,true);
    */
    
    cin>>pid;
    cout<<3<<" "<<3<<endl;
    rep(i,12) lg[1<<i]=i;
    for(;;){
        cin>>turn>>timeleft>>myscore>>opscore;
        rep(i,5)rep(j,5) cin>>mg[i][j];
        rep(i,5)rep(j,5) cin>>og[i][j];

        space=0;
        atk.clear();
        rep(i,5)rep(j,5){
            if(og[i][j]==0) atk.pb(i,j);
            if(mg[i][j]==0) ++space;
        }
        atksz=atk.size();


        ok=false;
        if(atksz==1||atksz==2){
            ok=true;
            rep(i,5)if(ok){
                rep(j,5){
                    rep(k,4){
                        int tx=j+dx[k],ty=i+dy[k];
                        if(tx>=0&&ty>=0&&tx<5&&ty<5&&og[i][j]==og[ty][tx])ok=false;
                    }
                }
            }
            if(ok){
                memset(ng,0,sizeof(ng));
                rep(i,atksz){
                    int cy=atk[i].first,cx=atk[i].second;
                    rep(k,4){
                        int tx=cx+dx[k],ty=cy+dy[k];
                        if(tx>=0&&ty>=0&&tx<5&&ty<5)ng[og[ty][tx]]=true;
                    }
                }
            }
        }
        bool win=false;
        simu(mg,true,false);
        anscom=-1;
        if(windir!=-1){
            win=true;
            anscom=windir;
            ansV=lg[(1<<(curV[windir]+1))/atksz];
            anspos.clear();
            rep(i,atksz){
                anspos.pb(atk[i]);
            }
            //cerr<<dir[anscom]<<" "<<ansM<<" "<<ansV<<" ";
            cerr<<"win!"<<endl;
        }
        
        bool lose=true;
        rep(i,4)if(curscore[i]!=-1) lose=false;

        if(lose){
            cerr<<"lose"<<endl;
            cout<<"U"<<" "<<1<<" "<<1<<" "<<3<<" "<<3<<endl;
            break;
        }

        ll mxval=-INF;
        rep(cdir,4)if(!win&&curscore[cdir]!=-1){

        int tmpV=curV[cdir]+1;
        
        rep(i,atksz)if(!win){
            og[atk[i].first][atk[i].second]=tmpV;
            ll cur=simu(og,false,false);
            if(cur==mnv){
                win=true;
                ansV=tmpV;
                anscom=cdir;
                anspos.clear();
                anspos.pb(atk[i]);
            }
            else if(mxval<myvalue[cdir]-cur){
                anspos.clear();
                mxval=myvalue[cdir]-cur;
                ansV=tmpV;
                anscom=cdir;
                anspos.pb(atk[i]);
            }
            og[atk[i].first][atk[i].second]=0;
        }
        if(!win&&tmpV>1&&atksz>1)rep(i,atksz)if(!win){
            FOR(j,i+1,atksz){
                og[atk[i].first][atk[i].second]=tmpV-1;
                og[atk[j].first][atk[j].second]=tmpV-1;
                ll cur=simu(og,false,false);
                if(cur==mnv){
                    win=true;
                    ansV=tmpV-1;
                    anscom=cdir;
                    anspos.clear();
                    anspos.pb(atk[i]);
                    anspos.pb(atk[j]);
                    break;
                }
                else if(mxval<myvalue[cdir]-cur){
                    anspos.clear();
                    mxval=myvalue[cdir]-cur;
                    anscom=cdir;
                    ansV=tmpV-1;
                    anspos.pb(atk[i]);
                    anspos.pb(atk[j]);
                }
                og[atk[i].first][atk[i].second]=0;
                og[atk[j].first][atk[j].second]=0;
            }
        }

        
        //for(int i=3;i>=2;--i){
        FOR(i,2,4){
            int ptcnt=(1<<i);
            if(win||tmpV<=i||atksz<(1<<i)) break;
            rep(j,200){
                shuffle();
                rep(k,ptcnt){
                    og[atk[k].first][atk[k].second]=tmpV-i;
                }
                ll cur=simu(og,false,false);
                if(cur==mnv){
                    win=true;
                    ansV=tmpV-i;
                    anscom=cdir;
                    anspos.clear();
                    rep(p,ptcnt) anspos.pb(atk[p]);
                    break;
                }
                else if(mxval<myvalue[cdir]-cur){
                    anspos.clear();
                    mxval=myvalue[cdir]-cur;
                    ansV=tmpV-i;
                    anscom=cdir;
                    rep(p,ptcnt) anspos.pb(atk[p]);
                }
                rep(k,ptcnt){
                    og[atk[k].first][atk[k].second]=0;
                }
            }
        }
        
        }
        totscore+=curscore[anscom];
        ansM=anspos.size();
        cout<<dir[anscom]<<" "<<ansM<<" "<<ansV<<" ";
        rep(i,ansM){
            cout<<anspos[i].first+1<<" "<<anspos[i].second+1<<" ";
        }
        cout<<endl;
        
        cerr<<"id: "<<pid<<" turn: "<<turn<<" myscore: "<<myscore<<"/"<<totscore<<" opscore: "<<opscore<<" timeleft: "<<timeleft<<endl;
        if(win) cerr<<pid<<" win"<<endl;
        if(turn==1000) break;
    }
    
    return 0;
}