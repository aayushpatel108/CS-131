import java.util.concurrent.atomic.AtomicLongArray;
public class AcmeSafeState implements State {
    private AtomicLongArray value;

    AcmeSafeState(int length) { value = new AtomicLongArray(length); }

    public int size() { return value.length(); }

    public long[] current() {
        current_value = new long[value.length()];
        for (int i = 0; i < value.length(); i++) {
            current_value[i] = (long)value.get(i);
        }
        return current_value;
    }

    public void swap(int i, int j) {
        value.getAndDecrement(i);
        value.getAndIncrement(j);
    }
}