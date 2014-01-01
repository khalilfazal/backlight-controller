package com.backlightcontroller;

import java.io.IOException;
import java.util.Locale;

import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpUriRequest;
import org.apache.http.impl.client.DefaultHttpClient;

import android.util.Log;

/**
 * The Class SendBrightness.
 *
 * @author Khalil Fazal
 */
public class SendBrightness extends Thread {

    /**
     * The request uri.
     */
    private final HttpUriRequest requestURI;

    /**
     * Instantiates a new send brightness.
     *
     * @param hostname the hostname
     * @param brightness the brightness
     */
    public SendBrightness(final String hostname, final double brightness) {
        final String name = this.getClass().getSimpleName();
        final String uri = String.format(Locale.CANADA, "%s%s%s%f", "http://", hostname, ":3000/", brightness);
        this.setName(String.format(Locale.CANADA, "%s%s%s", name, " ", uri));
        this.requestURI = new HttpPost(uri);
    }

    /**
     * Run.
     *
     * @see java.lang.Thread#run()
     */
    @Override
    public void run() {
        try {
            new DefaultHttpClient().execute(this.requestURI);
        } catch (final ClientProtocolException e) {
            this.throwMsg(e);
        } catch (final IOException e) {
            this.throwMsg(e);
        }
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