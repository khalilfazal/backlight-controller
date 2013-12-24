package com.backlightcontroller;

import java.io.IOException;

import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpUriRequest;
import org.apache.http.impl.client.DefaultHttpClient;

import android.util.Log;

/**
 * @author Khalil Fazal
 */
public class ChangeProgress extends Thread {

    /**
     * The progress.
     */
    private final double progress;

    /**
     * @param progress the current progress
     */
    public ChangeProgress(final double progress) {
        this.setName(this.getClass().getSimpleName());
        this.progress = progress;
    }

    /**
     * @see java.lang.Thread#run()
     */
    @Override
    public void run() {
        try {
            new DefaultHttpClient().execute(this.getPost());
        } catch (final ClientProtocolException e) {
            this.throwMsg(e);
        } catch (final IOException e) {
            this.throwMsg(e);
        }
    }

    /**
     * Gets the post uri.
     *
     * @return the post
     */
    private HttpUriRequest getPost() {
        return new HttpPost(String.format("%s%f", "http://99.243.174.190:3000/", this.progress));
    }

    /**
     * Throw msg.
     *
     * @param e the e
     */
    private void throwMsg(final Throwable e) {
        Log.e(this.getName(), e.getMessage());
    }

}
