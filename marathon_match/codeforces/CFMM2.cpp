#include <bits/stdc++.h>
#include <sys/time.h>
using namespace std;
#define FOR(i,a,b) for(int i=(a);i<(b);++i)
#define rep(i,n)   FOR(i,0,n)
#define pb emplace_back
typedef long long ll;
typedef pair<int,int> pint;

const int INF=1000100010;
double timeLimit=14700;
int start_time;
inline int get_time() {
  struct timeval tv;
  gettimeofday(&tv, NULL);
  return tv.tv_sec*1000+tv.tv_usec/1000;
}
int N,S,C,H,U;//100000,10000,10,10,10
const int MEMOSZ=18000;
string s_pattern,backpackst;
int backpack[10100],pattern[100001];
vector<int8_t> ans[MEMOSZ];
pint psmemo[MEMOSZ];
struct State{
    array<int,10> pos,hand_color;
    pint eval;
    int ansid;
    State(){};
    bool operator<(const State &r)const{return eval>r.eval;}
    bool operator==(const State &r)const{return eval==r.eval;}
};
int curId=0;
pint sorted_list[10];
int nextp[1000001];
State Beamsearch(int turn,State initState){
    vector<State> curStates,nextStates;
    curStates.pb(initState);
    int beamwidth=300;
    rep(i,turn){
        nextStates.clear();
        if(get_time()-start_time>timeLimit) beamwidth=50;
        rep(j,beamwidth){
            if(curStates.size()<=j) break;
            int sum=0;
            rep(k,10){
                sorted_list[k]={curStates[j].pos[k],k};
                sum+=curStates[j].pos[k];
            }
            sort(sorted_list,sorted_list+10);
            rep(a,6){ 
                int id=sorted_list[a].second;
                int curpos=sorted_list[a].first;
                rep(k,C)if(curStates[j].hand_color[k]>0){
                    State tmp=curStates[j];
                    int co=k,nx=curpos;
                    bool flag=true;
                    while(flag){
                        ++nx;
                        flag=false;
                        //while(pattern[nx%N]!=co)++nx;
                        //if(pattern[nx]!=co) nx+=nextp[co][nx];
                        if(pattern[nx]!=co) nx+=nextp[co+10*nx];
                        FOR(l,a+1,U){
                            if(sorted_list[l].first==nx){
                                flag=true;
                                break;
                            }
                        }
                                
                    }
                    if(nx-curpos<=6) continue;
                    int es=(sum+nx-curpos)*2/3;
                    if(a==0){
                        if(i+2<turn) tmp.eval={es,min(sorted_list[1].first,nx)};
                        else tmp.eval={min(sorted_list[1].first,nx),es};
                    }
                    else{
                        if(i+2<turn) tmp.eval={es,sorted_list[0].first};
                        else tmp.eval={sorted_list[0].first,es};
                    }
                    --tmp.hand_color[co];
                    ++tmp.hand_color[backpack[i+10]];
                    tmp.pos[id]=nx;

                    //ans[curId]=ans[tmp.ansid];
                    psmemo[curId]={tmp.ansid,id+10*co};
                    tmp.ansid=curId;
                    if(++curId==MEMOSZ) curId=0;
                    nextStates.pb(tmp);
                }
            }
        }
        
        swap(curStates,nextStates);
        sort(curStates.begin(),curStates.end());
        curStates.erase(unique(curStates.begin(),curStates.end()),curStates.end());

        rep(k,min(beamwidth,(int)curStates.size())){
            int tid=curStates[k].ansid;
            ans[tid]=ans[psmemo[tid].first];
            ans[tid].pb(psmemo[tid].second);
        }
        
    }
    //rep(i,curStates.size()) cerr<<curStates[i].eval.first<<" "<<curStates[i].eval.second<<endl;
    return curStates[0];
}
int memo[10];
int cnt[10];
int main(){
    start_time=get_time();
    ios::sync_with_stdio(false),cin.tie(0),cout.tie(0);
    cin>>N>>S>>C>>H>>U;
    cin>>s_pattern>>backpackst;
    rep(i,S+H){
        backpack[i]=backpackst[i]-'A';
    }
    //rep(i,N) pattern[i]=s_pattern[i]-'A';
    
    for(int i=N-1;i>=0;--i){
        pattern[i]=s_pattern[i]-'A';
        ++cnt[pattern[i]];
        memo[pattern[i]]=i;
        //rep(j,10) nextp[j][i]=memo[j]-i;
        rep(j,10) nextp[j+i*10]=memo[j]-i;
    }
    
    State initState;
    initState.eval={0,45};
    //initState.eval=45;
    rep(i,U) initState.pos[i]=i,initState.hand_color[i]=0;
    rep(i,H) ++initState.hand_color[backpack[i]];
    initState.ansid=curId;++curId;
    State best=Beamsearch(S,initState);

    rep(i,S) cout<<ans[best.ansid][i]%10<<" "<<(char)('A'+ans[best.ansid][i]/10)<<endl;

    //cerr<<best.eval.first<<" "<<best.eval.second<<endl;
    //cerr<<get_time()-start_time<<endl;
    return 0;
}