public class Swapper implements Runnable {
    private int offset;
    private Interval interval;
    private String content;
    private char[] buffer;

    public Swapper(Interval interval, String content, char[] buffer, int offset) {
        this.offset = offset;
        this.interval = interval;
        this.content = content;
        this.buffer = buffer;
    }

    //Offset: specifies the starting index in the buffer where the content will be placed.
    //Interval: specifies the starting and ending index of the content in the original file that is being swapped.
    //Content: the entire original file in a String.
    //Buffer: The shared char[] that the result is being written to.

    /*
     Write the specified content into the buffer. Helper methods may be used
     to retrieve the content and insert it into the proper spot in the buffer.
     */
    @Override
    public void run() {
        // TODO: Implement me!
        try {
            for(int i = 0; i < (this.interval.getY() - this.interval.getX()); i++){
                //this.buffer[offset + i] = this.content[this.interval.getX() + i];
                this.buffer[offset + i] = this.content.charAt(this.interval.getX() + i);
            }
            System.out.println(this.buffer);
            return;
        }
        catch (Exception e) {
            // Throwing an exception
            System.out.println ("Exception is caught");
        }
    }
}