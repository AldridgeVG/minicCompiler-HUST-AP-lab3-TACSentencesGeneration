int main(){
    int a;
    a=1;
    {
        int a=2;
        a=4;
    }
    return a;
}