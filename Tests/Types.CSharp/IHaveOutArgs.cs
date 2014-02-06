public interface IHaveOutArgs
{
    void NoInOneOutNoResult(out int x);
    bool NoInOneOutResult(out int x);
    bool OneInOneOutResult(int x, out int y);
    void NoInTwoOutNoResult(out int x, out int y);
    bool NoInThreeOutResult(out int x, out int y, out int z);
}

